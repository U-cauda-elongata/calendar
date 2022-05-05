module Main exposing (main)

import Browser exposing (Document)
import Browser.Dom as Dom
import Browser.Events exposing (onKeyDown)
import Calendar.Attributes exposing (..)
import Calendar.Elements exposing (intlDate, intlTime)
import Calendar.Event exposing (Event, feedDecoder)
import Calendar.Feeds as Feeds exposing (Feed)
import Calendar.Icon as Icon
import Calendar.Util exposing (toNaiveDate)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onInput)
import Http
import Http.Xml
import Json.Decode as D
import Task
import Time


main : Program () Model Msg
main =
    Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    { search : String
    , timeZone : Time.Zone
    , feeds : Dict String FeedState
    , errors : List String
    }


type alias FeedState =
    { feed : Feed
    , events : List Event
    , retrieving : FeedRetrievalState
    , checked : Bool
    }


type FeedRetrievalState
    = Retrieving
    | Success
    | Failure


type Msg
    = SearchInput String
    | FeedFilter String Bool
    | KeyDown String
    | SetTimeZone Time.Zone
    | GotFeed String (Result Http.Error (List Event))
    | NoOp
    | Unexpected String



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        ""
        Time.utc
        (Feeds.preset |> Dict.map (\_ -> \feed -> FeedState feed [] Retrieving True))
        []
    , Cmd.batch
        (Task.perform SetTimeZone Time.here
            :: (Feeds.preset
                    |> Dict.toList
                    |> List.map
                        (\( url, _ ) ->
                            Http.get
                                { url = url
                                , expect = Http.Xml.expectXml (GotFeed url) (feedDecoder url)
                                }
                        )
               )
        )
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchInput search ->
            ( { model | search = search }, Cmd.none )

        FeedFilter url checked ->
            ( { model
                | feeds =
                    model.feeds |> Dict.update url (Maybe.map (\fs -> { fs | checked = checked }))
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

        GotFeed url result ->
            case result of
                Ok events ->
                    ( { model
                        | feeds =
                            model.feeds
                                |> Dict.update url
                                    (Maybe.map
                                        (\fs -> { fs | events = events, retrieving = Success })
                                    )
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( { model
                        | errors =
                            model.errors
                                ++ [ "Error retrieving <" ++ url ++ ">: " ++ httpErrorToString err ]
                        , feeds =
                            model.feeds
                                |> Dict.update url
                                    (Maybe.map (\fs -> { fs | retrieving = Failure }))
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
    Task.attempt handleDomResult (Dom.focus "search")


blurSearch : Cmd Msg
blurSearch =
    Task.attempt handleDomResult (Dom.blur "search")


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
    D.map4 (\a -> \c -> \m -> \k -> KeyDown (a ++ c ++ m ++ k))
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
    , body = [ headerView model, mainView model, errorView model ]
    }


headerView : Model -> Html Msg
headerView model =
    header []
        [ div []
            [ input [ id "hamburger", class "hamburger-checkbox", type_ "checkbox", role "switch" ]
                []
            , label [ class "hamburger-label", for "hamburger", ariaHidden True ] [ Icon.hamburger ]
            , div [ class "drawer" ]
                [ menu [] [ searchView model, feedFilterView model ]
                , footer []
                    [ a
                        [ class "icon"
                        , href "https://github.com/U-cauda-elongata/calendar"
                        , rel "external"
                        ]
                        [ Icon.gitHub ]
                    ]
                ]
            ]
        , h1 [] [ text "けもフレ配信カレンダー" ]
        ]


searchView : Model -> Html Msg
searchView model =
    li []
        [ label []
            [ text "検索", input [ id "search", value model.search, onInput SearchInput ] [] ]
        ]


feedFilterView : Model -> Html Msg
feedFilterView model =
    li [ ariaLabelledby "feed-filter-header" ]
        [ header [ id "feed-filter-header" ] [ h2 [] [ text "チャンネル" ] ]
        , ul []
            (model.feeds
                |> Dict.toList
                |> List.map
                    (\( url, fs ) ->
                        li []
                            [ label []
                                [ input
                                    [ class "filter-checkbox"
                                    , type_ "checkbox"
                                    , onCheck (FeedFilter url)
                                    , checked fs.checked
                                    , disabled (fs.retrieving /= Success)
                                    ]
                                    []
                                , img [ class "avatar", src fs.feed.icon, alt fs.feed.title ] []
                                , p [] [ text fs.feed.title ]
                                ]
                            ]
                    )
            )
        ]


mainView : Model -> Html Msg
mainView model =
    main_
        [ ariaLive "polite"
        , ariaBusy
            (model.feeds
                |> Dict.foldl (\_ -> \fs -> \acc -> acc || fs.retrieving == Retrieving) False
            )
        ]
        (model.feeds
            |> Dict.values
            |> List.concatMap (\fs -> fs.events |> List.map (\e -> ( fs, e )))
            |> List.sortWith
                (\( _, e1 ) ->
                    \( _, e2 ) ->
                        compare (Time.posixToMillis e2.updated) (Time.posixToMillis e1.updated)
                )
            |> groupBy (\( _, event ) -> toNaiveDate model.timeZone event.updated)
            |> List.map
                (\( date, events ) ->
                    section []
                        [ header [ class "date-heading" ] [ intlDate date ]
                        , ul [ class "timeline" ]
                            (events |> List.map (\( feed, event ) -> eventView model feed event))
                        ]
                )
        )


eventView : Model -> FeedState -> Event -> Html Msg
eventView model fs event =
    let
        eventHeader =
            let
                heading =
                    h3 [] [ text event.name ]

                headerContent =
                    case event.thumbnail of
                        Just thumb ->
                            [ heading
                            , img
                                [ class "event-thumbnail"
                                , src thumb
                                , alt "サムネイル画像"
                                , loading "lazy"
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
        [ class "event", hidden (not (fs.checked && searchMatches model event)) ]
        [ intlTime event.updated
        , eventHeader
        , ul [ class "event-members" ]
            [ li [ class "event-member" ]
                [ a [ href fs.feed.alternate ]
                    [ img [ class "avatar", src fs.feed.icon, alt fs.feed.title ] [] ]
                ]
            ]
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


searchMatches : Model -> Event -> Bool
searchMatches model event =
    String.isEmpty model.search || String.contains model.search event.name


groupBy : (a -> b) -> List a -> List ( b, List a )
groupBy pred list =
    list
        |> List.foldr
            (\a ->
                \groups ->
                    let
                        b =
                            pred a
                    in
                    case groups of
                        ( bhead, as_ ) :: tail ->
                            if bhead == b then
                                ( bhead, a :: as_ ) :: tail

                            else
                                ( b, [ a ] ) :: groups

                        _ ->
                            [ ( b, [ a ] ) ]
            )
            []
