module Main exposing (main)

import Browser exposing (Document)
import Browser.Dom as Dom
import Browser.Events exposing (onKeyDown)
import Calendar.Attributes exposing (..)
import Calendar.Elements exposing (intlDate, intlTime)
import Calendar.Event exposing (Event, feedDecoder)
import Calendar.Feeds as Feeds
import Calendar.Icon as Icon
import Calendar.Util as Util
import Calendar.Util.NaiveDate as NaiveDate exposing (NaiveDate)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)
import Http
import Http.Xml
import Json.Decode as D
import List.Extra
import Regex
import Task
import Time


main : Program () Model Msg
main =
    Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    { search : String
    , timeZone : Time.Zone
    , feeds : List Feed
    , errors : List String
    }


type alias Feed =
    { meta : Feeds.Metadata
    , events : List Event
    , retrieving : FeedRetrievalState
    , checked : Bool
    }


type FeedRetrievalState
    = Retrieving
    | Success
    | Failure


type Msg
    = ClearFilter
    | SearchInput String
    | FeedFilter Int Bool
    | KeyDown String
    | SetTimeZone Time.Zone
    | GotFeed Int (Result Http.Error (List Event))
    | NoOp
    | Unexpected String



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        ""
        Time.utc
        (Feeds.preset |> List.map (\feed -> Feed feed [] Retrieving True))
        []
    , Cmd.batch
        (Task.perform SetTimeZone Time.here
            :: (Feeds.preset
                    |> List.indexedMap
                        (\i feed ->
                            Http.get
                                { url = feed.url
                                , expect =
                                    Http.Xml.expectXml (GotFeed i)
                                        (feedDecoder Feeds.preset feed)
                                }
                        )
               )
        )
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClearFilter ->
            ( { model
                | search = ""
                , feeds = model.feeds |> List.map (\feed -> { feed | checked = True })
              }
            , Cmd.none
            )

        SearchInput search ->
            ( { model | search = search }, Cmd.none )

        FeedFilter i checked ->
            ( { model
                | feeds =
                    model.feeds |> List.Extra.updateAt i (\feed -> { feed | checked = checked })
              }
            , Cmd.none
            )

        SetTimeZone timeZone ->
            ( { model | timeZone = timeZone }, Cmd.none )

        KeyDown key ->
            case key of
                "s" ->
                    ( model, focusSearch )

                "S" ->
                    ( model, focusSearch )

                "Escape" ->
                    ( model, blurSearch )

                _ ->
                    ( model, Cmd.none )

        GotFeed i result ->
            case result of
                Ok events ->
                    ( { model
                        | feeds =
                            model.feeds
                                |> List.Extra.updateAt i
                                    (\feed -> { feed | events = events, retrieving = Success })
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( { model
                        | errors =
                            let
                                url =
                                    model.feeds
                                        |> List.Extra.getAt i
                                        |> Maybe.map (\feed -> "<" ++ feed.meta.url ++ ">")
                                        -- This should never happen though.
                                        |> Maybe.withDefault "a feed"
                            in
                            model.errors
                                ++ [ "Error retrieving <" ++ url ++ ">: " ++ httpErrorToString err ]
                        , feeds =
                            model.feeds
                                |> List.Extra.updateAt i (\feed -> { feed | retrieving = Failure })
                      }
                    , Cmd.none
                    )

        NoOp ->
            ( model, Cmd.none )

        Unexpected err ->
            -- I'd prefer the application to simply crash in the event of a programming error which
            -- cannot be caught by the compiler like this, but Elm doesn't allow it.
            ( { model | errors = model.errors ++ [ "Unexpected error: " ++ err ] }, Cmd.none )


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl _ ->
            "BadUrl"

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "NetworkError"

        Http.BadStatus s ->
            "BadStatus " ++ String.fromInt s

        Http.BadBody _ ->
            "BadBody"


focusSearch : Cmd Msg
focusSearch =
    Task.attempt handleDomResult (Dom.focus "calendar-search")


blurSearch : Cmd Msg
blurSearch =
    Task.attempt handleDomResult (Dom.blur "calendar-search")


handleDomResult : Result Dom.Error value -> Msg
handleDomResult result =
    case result of
        Ok _ ->
            NoOp

        Err (Dom.NotFound id) ->
            Unexpected ("Node not found: " ++ id)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyDown keyDecoder


keyDecoder : D.Decoder Msg
keyDecoder =
    D.map4 (\a c m k -> KeyDown (a ++ c ++ m ++ k))
        (modifierDecoder "altKey" "A-")
        (modifierDecoder "ctrlKey" "C-")
        (modifierDecoder "metaKey" "M-")
        (D.field "key" D.string)


modifierDecoder : String -> String -> D.Decoder String
modifierDecoder field repr =
    D.oneOf [ D.field field D.bool, D.succeed False ]
        |> D.map
            (\value ->
                if value then
                    repr ++ "-"

                else
                    ""
            )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "けもフレ配信カレンダー"
    , body =
        [ input [ id "hamburger", class "hamburger-checkbox", type_ "checkbox", role "switch" ] []
        , label
            [ classList
                [ ( "hamburger-label", True )
                , ( "filter-active", filterApplied model )
                ]
            , for "hamburger"
            , ariaHidden True
            ]
            [ Icon.hamburger ]
        , header [] [ h1 [] [ text "けもフレ配信カレンダー" ] ]
        , drawerView model
        , div [ class "drawer-right" ] [ mainView model, errorView model ]
        ]
    }


drawerView : Model -> Html Msg
drawerView model =
    div [ class "drawer" ]
        [ menu [ ariaLabel "フィルターツール" ]
            [ button
                [ class "filter-clear-button"
                , disabled (not (filterApplied model))
                , onClick ClearFilter
                ]
                [ text "フィルターをクリアー" ]
            , searchView model
            , feedFilterView model
            ]
        , footer []
            [ a [ class "icon", href "https://github.com/U-cauda-elongata/calendar" ]
                [ Icon.gitHub ]
            ]
        ]


filterApplied : Model -> Bool
filterApplied model =
    model.search /= "" || not (model.feeds |> List.all .checked)


searchView : Model -> Html Msg
searchView model =
    li []
        [ label []
            [ h2 [] [ text "検索" ]
            , input
                [ id "calendar-search"
                , type_ "search"
                , value model.search
                , list "searchlist"
                , onInput SearchInput
                ]
                []
            ]
        , datalist [ id "searchlist" ]
            (model.feeds
                |> List.concatMap .events
                |> List.concatMap (\event -> searchTags event.name)
                |> Util.cardinalities
                |> Dict.toList
                |> List.sortBy (\( _, n ) -> -n)
                |> List.map (\( tag, _ ) -> option [ value tag ] [])
            )
        ]


tagRe : Regex.Regex
tagRe =
    Regex.fromString "【([^】]*)】" |> Maybe.withDefault Regex.never


slashesRe : Regex.Regex
slashesRe =
    Regex.fromString "[/／]" |> Maybe.withDefault Regex.never


searchTags : String -> List String
searchTags string =
    Regex.find tagRe string
        |> List.filterMap (\match -> List.head match.submatches |> Maybe.andThen identity)
        |> List.concatMap (Regex.split slashesRe)
        |> List.map String.trim


feedFilterView : Model -> Html Msg
feedFilterView model =
    li [ ariaLabelledby "feed-filter-heading" ]
        [ h2 [ id "feed-filter-heading" ] [ text "チャンネル" ]
        , ul []
            (model.feeds
                |> List.indexedMap
                    (\i feed ->
                        let
                            pId =
                                "feed-" ++ String.fromInt i
                        in
                        li []
                            [ label [ role "switch", ariaLabelledby pId ]
                                [ input
                                    [ class "filter-checkbox"
                                    , type_ "checkbox"
                                    , onCheck (FeedFilter i)
                                    , checked feed.checked
                                    , disabled (feed.retrieving /= Success)
                                    ]
                                    []
                                , img [ class "avatar", src feed.meta.icon, alt "アイコン画像" ] []
                                , p [ id pId ] [ text feed.meta.title ]
                                ]
                            ]
                    )
            )
        ]


mainView : Model -> Html Msg
mainView model =
    main_
        [ ariaLive "polite"
        , ariaBusy (model.feeds |> List.any (\feed -> feed.retrieving == Retrieving))
        ]
        (model.feeds
            |> List.concatMap (\feed -> feed.events |> List.map (\e -> ( feed, e )))
            |> List.sortWith
                (\( _, e1 ) ( _, e2 ) ->
                    compare (Time.posixToMillis e2.time) (Time.posixToMillis e1.time)
                )
            |> Util.groupBy (\( _, event ) -> NaiveDate.fromPosix model.timeZone event.time)
            |> List.map
                (\( date, events ) ->
                    section
                        [ hidden
                            (not
                                (events
                                    |> List.any (\( feed, event ) -> eventIsShown model feed event)
                                )
                            )
                        ]
                        [ header [ class "date-heading" ] [ intlDate [] date ]
                        , ul []
                            (events
                                |> List.indexedMap
                                    (\i ( feed, event ) -> eventView model date i feed event)
                            )
                        ]
                )
        )


eventView : Model -> NaiveDate -> Int -> Feed -> Event -> Html Msg
eventView model date i feed event =
    let
        headingId =
            "event-" ++ NaiveDate.toIso8601 date ++ "-" ++ String.fromInt i

        eventHeader =
            let
                heading =
                    h3 [ id headingId ] [ text event.name ]

                headerContent =
                    case event.thumbnail of
                        Just thumb ->
                            [ heading
                            , img
                                [ class "event-thumbnail"
                                , loading "lazy"
                                , src thumb
                                , alt "サムネイル画像"
                                ]
                                []
                            ]

                        Nothing ->
                            [ heading ]
            in
            case event.link of
                Just link ->
                    header []
                        [ a [ class "event-header-grid", href link ] headerContent ]

                Nothing ->
                    header [ class "event-header-grid" ] headerContent
    in
    li
        [ class "event"
        , role "article"
        , ariaLabelledby headingId
        , hidden (not (eventIsShown model feed event))
        ]
        [ intlTime [ class "event-time" ] event.time
        , eventHeader
        , ul [ class "event-members" ]
            (eventMemberView True feed
                :: (event.members
                        |> List.filterMap (\memberIdx -> model.feeds |> List.Extra.getAt memberIdx)
                        |> List.map (eventMemberView False)
                   )
            )
        ]


eventIsShown : Model -> Feed -> Event -> Bool
eventIsShown model feed event =
    (feed.checked
        || -- Check that any of the members' feed is checked.
           (model.feeds
                |> List.Extra.indexedFoldl
                    (\i f any -> any || (f.checked && (event.members |> List.member i)))
                    False
           )
    )
        && searchMatches model event


searchMatches : Model -> Event -> Bool
searchMatches model event =
    String.isEmpty model.search || String.contains model.search event.name


eventMemberView : Bool -> Feed -> Html Msg
eventMemberView isAuthor feed =
    li [ class "event-member" ]
        [ a
            (href feed.meta.alternate
                :: (if isAuthor then
                        [ rel "author" ]

                    else
                        []
                   )
            )
            [ img [ class "avatar", src feed.meta.icon, alt feed.meta.title ] [] ]
        ]


errorView : Model -> Html Msg
errorView model =
    div
        [ class "error-log"
        , role "log"
        , ariaLive "assertive"
        , ariaLabel "Error"
        , lang "en"
        , hidden (List.isEmpty model.errors)
        ]
        (List.map (\msg -> p [] [ text msg ]) model.errors)
