port module Main exposing (main)

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
import Html.Events exposing (onBlur, onClick, onFocus, onInput)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2, lazy6)
import Http
import I18Next exposing (Translations, translationsDecoder)
import Json.Decode as D
import List.Extra
import Regex
import Task
import Time
import Translations as T
import Translations.Error as TError
import Translations.Share as TShare


main : Program Flags Model Msg
main =
    Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    { features : Features
    , translations : Translations
    , search : String
    , now : Time.Posix
    , timeZone : Time.Zone
    , feeds : List Feed
    , searchFocused : Bool
    , activePopup : Maybe ( Int, Int )
    , errors : List Error
    }


type alias Features =
    { copy : Bool
    , share : Bool
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


type Error
    = HttpError String Http.Error
    | Unexpected String


type Msg
    = ClearFilter
    | SearchInput String
    | ToggleFeedFilter Int Bool
    | KeyDown String
    | SearchFocus Bool
    | OpenPopup ( Int, Int )
    | ClosePopup
    | SetTimeZone Time.Zone
    | GotCurrentTime Time.Posix
    | GotTranslations String (Result Http.Error Translations)
    | GotFeed Int String (Result Http.Error (List Event))
    | Copy String
    | Share String (Maybe String)
    | NoOp
    | ReportError Error



-- PORTS


port setLang : String -> Cmd msg


port slideViewportInto : String -> Cmd msg


port copy : String -> Cmd msg


port share : ShareData -> Cmd msg


type alias ShareData =
    { title : String
    , url : Maybe String
    }



-- INIT


type alias Flags =
    { features : Features
    , languages : List String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model
        flags.features
        I18Next.initialTranslations
        ""
        (Time.millisToPosix 0)
        Time.utc
        (Feeds.preset |> List.map (\feed -> Feed feed [] Retrieving True))
        False
        Nothing
        []
    , Cmd.batch
        ((Time.here |> Task.perform SetTimeZone)
            :: (let
                    lang =
                        selectLanguage flags.languages
                in
                Http.get
                    { url = translationsUrl lang
                    , expect = Http.expectJson (GotTranslations lang) translationsDecoder
                    }
               )
            :: (Time.now |> Task.perform GotCurrentTime)
            :: (Feeds.preset
                    |> List.indexedMap
                        (\i feed ->
                            Http.get
                                { url = feed.url
                                , expect =
                                    Http.expectJson (GotFeed i feed.url) (feedDecoder Feeds.preset)
                                }
                        )
               )
        )
    )


selectLanguage : List String -> String
selectLanguage languages =
    case languages of
        [] ->
            "en"

        lang :: rest ->
            if lang == "en" || (lang |> String.startsWith "en-") then
                "en"

            else if lang == "ja" || (lang |> String.startsWith "ja-") then
                "ja"

            else
                selectLanguage rest


translationsUrl : String -> String
translationsUrl lang =
    "translations/" ++ lang ++ ".json"



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

        ToggleFeedFilter i checked ->
            ( { model
                | feeds =
                    model.feeds |> List.Extra.updateAt i (\feed -> { feed | checked = checked })
              }
            , Cmd.none
            )

        SetTimeZone timeZone ->
            ( { model | timeZone = timeZone }, Cmd.none )

        GotCurrentTime now ->
            ( { model | now = now }, Cmd.none )

        GotTranslations lang result ->
            case result of
                Ok translations ->
                    ( { model | translations = translations }, setLang lang )

                Err err ->
                    model |> update (ReportError (HttpError (translationsUrl lang) err))

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

        SearchFocus value ->
            ( { model | searchFocused = value }
            , -- Prevent `.drawer` to scroll into the search input before the transition completes.
              Dom.setViewportOf "drawer" 0 0 |> Task.attempt handleDomResult
            )

        OpenPopup idx ->
            ( { model | activePopup = Just idx }, Cmd.none )

        ClosePopup ->
            ( { model | activePopup = Nothing }, Cmd.none )

        GotFeed i url result ->
            -- XXX: #lm prohibits shadowing.
            let
                model2 =
                    case result of
                        Ok events ->
                            { model
                                | feeds =
                                    model.feeds
                                        |> List.Extra.updateAt i
                                            (\feed ->
                                                { feed | events = events, retrieving = Success }
                                            )
                            }

                        Err err ->
                            { model
                                | feeds =
                                    model.feeds
                                        |> List.Extra.updateAt i
                                            (\feed -> { feed | retrieving = Failure })
                                , errors = HttpError url err :: model.errors
                            }
            in
            ( model2
            , if model2.feeds |> List.all (\feed -> feed.retrieving /= Retrieving) then
                slideViewportInto "now"

              else
                Cmd.none
            )

        Copy text ->
            ( model, copy text )

        Share title url ->
            ( model, share <| ShareData title url )

        NoOp ->
            ( model, Cmd.none )

        ReportError err ->
            ( { model | errors = err :: model.errors }, Cmd.none )


focusSearch : Cmd Msg
focusSearch =
    Dom.focus "calendar-search" |> Task.attempt handleDomResult


blurSearch : Cmd Msg
blurSearch =
    Dom.blur "calendar-search" |> Task.attempt handleDomResult


handleDomResult : Result Dom.Error value -> Msg
handleDomResult result =
    case result of
        Ok _ ->
            NoOp

        Err (Dom.NotFound id) ->
            ReportError <| Unexpected ("Node not found: " ++ id)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown keyDecoder
        , model.activePopup
            |> Maybe.map (always <| Browser.Events.onClick (D.succeed ClosePopup))
            |> Maybe.withDefault Sub.none
        ]


keyDecoder : D.Decoder Msg
keyDecoder =
    D.field "key" D.string
        |> D.andThen (appendModifier "metaKey" "M-")
        |> D.andThen (appendModifier "ctrlKey" "C-")
        |> D.andThen (appendModifier "altKey" "A-")
        |> D.map KeyDown


appendModifier : String -> String -> String -> D.Decoder String
appendModifier field prefix key =
    D.oneOf [ D.field field D.bool, D.succeed False ]
        |> D.map
            (\value ->
                if value then
                    prefix ++ key

                else
                    key
            )



-- VIEW


view : Model -> Document Msg
view model =
    { title = T.title model.translations
    , body =
        [ input
            [ id "hamburger"
            , classList
                [ ( "hamburger-checkbox", True )
                , -- XXX: Maybe this should be set to `<body>` ideally?
                  ( "search-focused", model.searchFocused )
                ]
            , type_ "checkbox"
            ]
            []
        , label
            [ classList
                [ ( "hamburger-label", True )
                , ( "filter-active", filterApplied model )
                ]
            , for "hamburger"
            , ariaHidden True
            ]
            [ Icon.hamburger ]
        , header [ class "drawer-right" ] [ h1 [] [ text (T.title model.translations) ] ]
        , div [ id "drawer", class "drawer-container" ] [ viewDrawer model ]
        , div [ class "drawer-right" ]
            [ viewMain model
            , lazy2 viewErrorLog model.translations model.errors
            ]
        ]
    }


viewDrawer : Model -> Html Msg
viewDrawer model =
    div [ class "drawer" ]
        [ menu [ ariaLabel (T.filterMenuLabel model.translations) ]
            [ li []
                [ button
                    [ class "filter-clear-button"
                    , disabled <| not (filterApplied model)
                    , onClick ClearFilter
                    , ariaLabel <| T.clearFilter model.translations
                    ]
                    [ Icon.clear ]
                ]
            , viewSearch model
            , hr [] []
            , viewFeedFilter model
            ]
        , footer []
            [ a [ class "icon", href "https://github.com/U-cauda-elongata/calendar" ]
                [ Icon.gitHub ]
            ]
        ]


filterApplied : Model -> Bool
filterApplied model =
    model.search /= "" || not (model.feeds |> List.all .checked)


viewSearch : Model -> Html Msg
viewSearch model =
    li []
        [ label [ class "search-label" ]
            [ lazy Icon.search model.translations
            , input
                [ id "calendar-search"
                , type_ "search"
                , value model.search
                , list "searchlist"
                , onInput SearchInput
                , onFocus (SearchFocus True)
                , onBlur (SearchFocus False)
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


viewFeedFilter : Model -> Html Msg
viewFeedFilter model =
    li [ ariaLabel (T.feedFilterLabel model.translations) ]
        [ ul []
            (model.feeds
                |> List.indexedMap
                    (\i feed ->
                        let
                            pId =
                                "feed-" ++ String.fromInt i
                        in
                        li [ class "filter-item" ]
                            [ button
                                [ class "filter-button"
                                , role "switch"
                                , title feed.meta.title
                                , onClick (ToggleFeedFilter i (not feed.checked))
                                , checked feed.checked
                                , disabled (feed.retrieving /= Success)
                                , ariaChecked feed.checked
                                , ariaLabelledby pId
                                ]
                                [ img
                                    [ class "avatar"
                                    , src feed.meta.icon
                                    , alt (T.avatarAlt model.translations)
                                    ]
                                    []
                                , p [ id pId, class "filter-label" ] [ text feed.meta.title ]
                                ]
                            ]
                    )
            )
        ]


type TimelineItem
    = TimelineEvent ( ( Int, Int ), Feed, Event )
    | Now


viewMain : Model -> Html Msg
viewMain model =
    let
        busy =
            model.feeds |> List.any (\feed -> feed.retrieving == Retrieving)
    in
    Keyed.node "main"
        [ ariaLive "polite"
        , ariaBusy busy
        ]
        (let
            anyEventIsShown =
                model.feeds
                    |> List.any (\feed -> feed.events |> List.any (eventIsShown model feed.checked))

            events =
                model.feeds
                    |> List.indexedMap Tuple.pair
                    |> List.concatMap
                        (\( feedIdx, feed ) ->
                            feed.events |> List.indexedMap (\i e -> ( ( feedIdx, i ), feed, e ))
                        )
                    |> List.sortWith
                        (\( _, _, e1 ) ( _, _, e2 ) ->
                            compare (Time.posixToMillis e2.time) (Time.posixToMillis e1.time)
                        )

            ( upcoming, past ) =
                events
                    |> List.Extra.splitWhen
                        (\( _, _, event ) ->
                            Time.posixToMillis event.time <= Time.posixToMillis model.now
                        )
                    |> Maybe.withDefault ( events, [] )
                    |> Tuple.mapBoth (List.map TimelineEvent) (List.map TimelineEvent)
         in
         ((upcoming ++ (Now :: past))
            |> Util.groupBy
                (\item ->
                    case item of
                        TimelineEvent ( _, _, event ) ->
                            NaiveDate.fromPosix model.timeZone event.time

                        Now ->
                            NaiveDate.fromPosix model.timeZone model.now
                )
            |> List.map (\( date, items ) -> viewKeyedDateSection model date items)
         )
            ++ [ ( "empty"
                 , div [ class "empty-result", hidden <| busy || anyEventIsShown ]
                    (let
                        pre =
                            p [] [ text <| T.emptyResultPre model.translations ]

                        post =
                            p [] [ text <| T.emptyResultPost model.translations ]
                     in
                     case T.emptyResultKidding model.translations of
                        "" ->
                            [ pre, post ]

                        mid ->
                            [ pre, p [] [ del [] [ text mid ] ], post ]
                    )
                 )
               ]
        )


viewKeyedDateSection : Model -> NaiveDate -> List TimelineItem -> ( String, Html Msg )
viewKeyedDateSection model date items =
    ( NaiveDate.toIso8601 date, viewDateSection model date items )


viewDateSection : Model -> NaiveDate -> List TimelineItem -> Html Msg
viewDateSection model date items =
    section
        [ hidden <|
            not
                (items
                    |> List.any
                        (\item ->
                            case item of
                                TimelineEvent ( _, feed, event ) ->
                                    eventIsShown model feed.checked event

                                Now ->
                                    True
                        )
                )
        ]
        [ header [ class "date-heading" ] [ intlDate [] date ]
        , Keyed.ul [ class "timeline" ]
            (items
                |> List.map
                    (\item ->
                        case item of
                            TimelineEvent ( id, feed, event ) ->
                                viewKeyedEvent model id feed event

                            Now ->
                                ( "now"
                                , div [ id "now", class "now-separator" ]
                                    [ p [] <|
                                        T.nowSeparatorCustom model.translations
                                            text
                                            (intlTime [] model.now)
                                    ]
                                )
                    )
            )
        ]


viewKeyedEvent : Model -> ( Int, Int ) -> Feed -> Event -> ( String, Html Msg )
viewKeyedEvent model ( feedIdx, eventIdx ) feed event =
    let
        eventId =
            "event-" ++ String.fromInt feedIdx ++ "-" ++ String.fromInt eventIdx

        headingId =
            eventId ++ "-heading"

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
                                , alt (T.thumbnailAlt model.translations)
                                ]
                                []
                            ]

                        Nothing ->
                            [ heading ]
            in
            event.link
                |> Maybe.map
                    (\link ->
                        header [] [ a [ class "event-header-grid", href link ] headerContent ]
                    )
                |> Maybe.withDefault (header [ class "event-header-grid" ] headerContent)
    in
    ( eventId
    , li
        [ class "event"
        , role "article"
        , ariaLabelledby headingId
        , hidden <| not (eventIsShown model feed.checked event)
        ]
        (intlTime [ class "event-time" ] event.time
            :: eventHeader
            :: ul [ class "event-members" ]
                (viewEventMember True feed
                    :: (event.members
                            |> List.filterMap
                                (\memberIdx ->
                                    if feedIdx == memberIdx then
                                        -- Exclude the author.
                                        Nothing

                                    else
                                        model.feeds
                                            |> List.Extra.getAt memberIdx
                                            |> Maybe.map (viewEventMember False)
                                )
                       )
                )
            :: (if model.features.copy || model.features.share then
                    List.singleton <|
                        lazy6 viewEventPopup
                            model.features
                            model.translations
                            ( feedIdx, eventIdx )
                            eventId
                            (model.activePopup == Just ( feedIdx, eventIdx ))
                            event

                else
                    []
               )
        )
    )


viewEventPopup : Features -> Translations -> ( Int, Int ) -> String -> Bool -> Event -> Html Msg
viewEventPopup features translations idx key expanded event =
    let
        popupId =
            key ++ "-popup"
    in
    div
        [ class "popup-container" ]
        [ button
            [ class "popup-toggle"
            , ariaHaspopup "menu"
            , ariaControls popupId
            , ariaExpanded expanded
            , -- Call `stopPropagation` to prevent `ClosePopup` message to be sent.
              Html.Events.stopPropagationOn "click" <|
                D.succeed
                    ( if expanded then
                        ClosePopup

                      else
                        OpenPopup idx
                    , True
                    )
            , ariaLabel "共有"
            ]
            -- TODO: Add an icon.
            [ text "…" ]
        , menu
            [ id popupId
            , classList [ ( "popup", True ), ( "expanded", expanded ) ]
            , ariaLabel "共有"
            ]
            -- TODO: Add icons to list items too.
            (let
                items =
                    []

                items2 =
                    if features.share then
                        viewShareEvent translations event :: items

                    else
                        items

                items3 =
                    if features.copy then
                        viewCopyEventTimestamp translations event
                            :: viewCopyEvent translations event
                            :: items2

                    else
                        items2
             in
             items3
            )
        ]


viewCopyEvent : Translations -> Event -> Html Msg
viewCopyEvent translations event =
    let
        copyText =
            event.link
                |> Maybe.map (\link -> event.name ++ "\n" ++ link)
                |> Maybe.withDefault event.name
    in
    li []
        [ button
            [ onClick <| Copy copyText ]
            [ text <| TShare.copyTitleAndUrl translations ]
        ]


viewCopyEventTimestamp : Translations -> Event -> Html Msg
viewCopyEventTimestamp translations event =
    li []
        [ button
            [ onClick <| Copy (String.fromInt (Time.posixToMillis event.time // 1000)) ]
            [ text <| TShare.copyTimestamp translations ]
        ]


viewShareEvent : Translations -> Event -> Html Msg
viewShareEvent translations event =
    li []
        [ button
            [ onClick <| Share event.name event.link ]
            [ text <| TShare.shareVia translations ]
        ]


eventIsShown : Model -> Bool -> Event -> Bool
eventIsShown model feedChecked event =
    (feedChecked
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


viewEventMember : Bool -> Feed -> Html Msg
viewEventMember isAuthor feed =
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


viewErrorLog : Translations -> List Error -> Html Msg
viewErrorLog translations errors =
    div
        [ class "error-log"
        , role "log"
        , ariaLive "assertive"
        , ariaLabel "Error"
        , hidden (List.isEmpty errors)
        ]
        (errors |> List.foldl (\err acc -> p [] (viewError translations err) :: acc) [])


viewError : Translations -> Error -> List (Html msg)
viewError translations err =
    case err of
        HttpError url e ->
            TError.httpCustom translations
                text
                (a [ href url ] [ text url ])
                (text <| httpErrorToString e)

        Unexpected msg ->
            -- I'd prefer the application to simply crash in the event of a programming error which
            -- cannot be caught by the compiler like this, but Elm doesn't allow it.
            [ text <| TError.unexpected translations msg ]


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

        Http.BadBody e ->
            "BadBody: " ++ e
