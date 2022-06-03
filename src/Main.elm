port module Main exposing (main)

import Browser exposing (Document)
import Browser.Dom as Dom
import Browser.Events exposing (onKeyDown)
import Calendar.Attributes exposing (..)
import Calendar.Elements exposing (..)
import Calendar.Event as Event exposing (Event)
import Calendar.Feed as Feed
import Calendar.Icon as Icon
import Calendar.TranslationsExt as T
import Calendar.Util as Util
import Calendar.Util.Dict as DictUtil
import Calendar.Util.Duration as Duration
import Calendar.Util.NaiveDate as NaiveDate exposing (NaiveDate)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onClick, onFocus, onInput)
import Html.Keyed as Keyed
import Html.Lazy exposing (..)
import Http
import I18Next exposing (Translations, translationsDecoder)
import Json.Decode as D
import List.Extra
import Markdown
import Process
import Regex
import Set exposing (Set)
import Svg.Attributes
import Task
import Time
import Translations as T
import Translations.About as TAbout
import Translations.Error as TError
import Translations.Event as TEvent
import Translations.Event.Description as TEventDescription
import Translations.Share as TShare
import Url.Builder


main : Program Flags Model Msg
main =
    Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    { features : Features
    , translations : Translations
    , search : String
    , now : Time.Posix
    , timeZone : Time.Zone
    , -- Set this to `True` once the first feed request completes, in order to prevent subsequent
      -- requests from causing `slideViewportInto` to be called again.
      initialized : Bool
    , feeds : List Feed
    , latestNumberedPage : Maybe String
    , gotPages : Set String
    , events : List Event
    , pendingFeed : PendingFeed
    , mode : Mode
    , copying : Maybe (Result Http.Error (Html Msg))
    , searchFocused : Bool
    , activePopup : Maybe String
    , errors : List Error
    }


type alias Features =
    { copy : Bool
    , share : Bool
    }


type alias Feed =
    { preset : Feed.Preset
    , alternate : String
    , checked : Bool
    }


type PendingFeed
    = OneMore String
    | Retry String
    | Loading
    | Done


type Mode
    = None
    | About AboutView


type AboutView
    = AboutMain
    | AboutCopying


type Error
    = FeedHttpError String Http.Error
    | TranslationsHttpError String Http.Error
    | Unexpected String


type PollingKind
    = Initial
    | Manual
    | AutoRefresh
    | Backfill (Maybe String)


type Msg
    = ClearFilter
    | SearchInput String
    | ToggleFeedFilter String Bool
    | KeyDown String
    | SearchFocus Bool
    | GetFeed String
    | Refresh
    | SetMode Mode
    | CloseWidgets
    | AboutBackToMain
    | AboutOpenCopying
    | AboutGotCopying (Result Http.Error String)
    | AboutRetryGetCopying
    | OpenPopup String
    | ClosePopup
    | SetTimeZone Time.Zone
    | Tick Time.Posix
    | GotTranslations String (Result Http.Error Translations)
    | GotFeed PollingKind String (Result Http.Error Feed.Feed)
    | DismissError Int
    | RetryGetTranslations String Int
    | RetryGetFeed String
    | Copy String
    | Share String (Maybe String)
    | NoOp
    | ReportError Error



-- PORTS


port setLang : String -> Cmd msg


port slideViewportInto : String -> Cmd msg


port showModal : String -> Cmd msg


port close : String -> Cmd msg


port copy : String -> Cmd msg


port share : ShareData -> Cmd msg


port onScrollToBottom : (() -> msg) -> Sub msg


port removeScrollEventListener : () -> Cmd msg


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
        initialTranslations
        ""
        (Time.millisToPosix 0)
        Time.utc
        False
        (Feed.presets |> List.map (\feed -> Feed feed "" True))
        Nothing
        Set.empty
        []
        Loading
        None
        Nothing
        False
        Nothing
        []
    , Cmd.batch
        [ Time.here |> Task.perform SetTimeZone
        , getTranslations <| selectLanguage flags.languages
        , Time.now |> Task.perform Tick
        , getFeed Initial "latest.json"
        ]
    )


initialTranslations : Translations
initialTranslations =
    I18Next.fromTree
        [ ( "title", I18Next.string "" )
        , ( "nowSeparator", I18Next.string "{{time}}" )
        ]


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


getTranslations : String -> Cmd Msg
getTranslations lang =
    Http.get
        { url = translationsUrl lang
        , expect = Http.expectJson (GotTranslations lang) translationsDecoder
        }


getFeed : PollingKind -> String -> Cmd Msg
getFeed polling url =
    Http.get
        { url = Url.Builder.relative [ "feed", url ] []
        , expect = Http.expectJson (GotFeed polling url) Feed.decoder
        }



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

        ToggleFeedFilter id checked ->
            ( { model
                | feeds =
                    model.feeds
                        |> List.map
                            (\feed ->
                                if feed.preset.id == id then
                                    { feed | checked = checked }

                                else
                                    feed
                            )
              }
            , Cmd.none
            )

        SetTimeZone timeZone ->
            ( { model | timeZone = timeZone }, Cmd.none )

        Tick now ->
            let
                interval =
                    if model.events |> List.any Event.isOngoing then
                        -- An ongoing event need to show the duration elapsed since the start time
                        -- in one-second precision.
                        1000

                    else
                        60000

                nextTick =
                    interval - (Time.posixToMillis now |> modBy interval)
            in
            ( { model | now = now }
            , Process.sleep (nextTick |> toFloat)
                |> Task.andThen (\() -> Time.now)
                |> Task.perform Tick
            )

        GotTranslations lang result ->
            case result of
                Ok translations ->
                    ( { model | translations = translations }, setLang lang )

                Err err ->
                    model |> update (ReportError (TranslationsHttpError lang err))

        DismissError errIdx ->
            ( { model | errors = model.errors |> List.Extra.removeAt errIdx }, Cmd.none )

        RetryGetTranslations lang errIdx ->
            ( { model | errors = model.errors |> List.Extra.removeAt errIdx }
            , getTranslations lang
            )

        KeyDown key ->
            case key of
                "s" ->
                    ( model, focusSearch )

                "S" ->
                    ( model, focusSearch )

                "Escape" ->
                    update CloseWidgets model
                        |> Tuple.mapSecond (\cmd -> Cmd.batch [ cmd, blurSearch ])

                _ ->
                    ( model, Cmd.none )

        SearchFocus value ->
            ( { model | searchFocused = value }
            , -- Prevent `.drawer` to scroll into the search input before the transition completes.
              Dom.setViewportOf "drawer" 0 0 |> Task.attempt handleDomResult
            )

        GetFeed url ->
            ( { model | pendingFeed = Loading }, getFeed Manual url )

        Refresh ->
            ( model, getFeed AutoRefresh "latest.json" )

        SetMode mode ->
            ( { model | mode = mode, activePopup = Nothing }
            , case ( model.mode, mode ) of
                ( None, About _ ) ->
                    Cmd.batch
                        [ showModal "about"
                        , Dom.focus "about-close-button" |> Task.attempt handleDomResult
                        ]

                ( About _, None ) ->
                    Cmd.batch
                        [ close "about"
                        , Dom.focus "about-button" |> Task.attempt handleDomResult
                        ]

                _ ->
                    Cmd.none
            )

        CloseWidgets ->
            update (SetMode None) { model | activePopup = Nothing }

        AboutBackToMain ->
            ( { model | mode = About AboutMain }, Cmd.none )

        AboutOpenCopying ->
            ( { model | mode = About AboutCopying }
            , case model.copying of
                Just _ ->
                    Cmd.none

                Nothing ->
                    getCopying
            )

        AboutGotCopying result ->
            ( { model | copying = Just <| Result.map (Markdown.toHtml [ class "copying" ]) result }
            , Cmd.none
            )

        AboutRetryGetCopying ->
            ( model, getCopying )

        OpenPopup idx ->
            ( { model | activePopup = Just idx }, Cmd.none )

        ClosePopup ->
            ( { model | activePopup = Nothing }, Cmd.none )

        GotFeed polling url result ->
            -- XXX: #lm prohibits shadowing.
            let
                ( model2, cmd ) =
                    case result of
                        Ok { meta, entries, next } ->
                            let
                                updateEvents oldEvents newEvents =
                                    -- Replace existing event if any, or insert new one if not.
                                    -- XXX: This could be done more efficiently if there'd be an
                                    -- implementation of `Dict` that preserves ordering.
                                    newEvents
                                        |> List.foldl
                                            (\ne acc1 ->
                                                let
                                                    ( acc, replaced ) =
                                                        acc1
                                                            |> List.foldl
                                                                (\oe ( acc2, r ) ->
                                                                    if ne.id == oe.id then
                                                                        ( ne :: acc2, True )

                                                                    else
                                                                        ( oe :: acc2, r )
                                                                )
                                                                ( [], False )
                                                in
                                                if replaced then
                                                    acc

                                                else
                                                    ne :: acc
                                            )
                                            oldEvents

                                events =
                                    (case polling of
                                        AutoRefresh ->
                                            updateEvents model.events entries

                                        Backfill _ ->
                                            updateEvents model.events entries

                                        _ ->
                                            entries ++ model.events
                                    )
                                        |> List.sortWith
                                            (\e1 e2 ->
                                                compare
                                                    (Time.posixToMillis e2.time)
                                                    (Time.posixToMillis e1.time)
                                            )
                            in
                            ( { model
                                | feeds =
                                    meta
                                        |> List.foldl
                                            (\m acc ->
                                                acc
                                                    |> List.map
                                                        (\feed ->
                                                            let
                                                                preset =
                                                                    feed.preset
                                                            in
                                                            if preset.id == m.id then
                                                                { feed
                                                                    | preset =
                                                                        { preset | title = m.title }
                                                                    , alternate = m.alternate
                                                                }

                                                            else
                                                                feed
                                                        )
                                            )
                                            model.feeds
                                , latestNumberedPage =
                                    if url == "latest.json" then
                                        next

                                    else
                                        model.latestNumberedPage
                                , gotPages = model.gotPages |> Set.insert url
                                , events = events
                                , pendingFeed =
                                    if polling == Initial || polling == Manual then
                                        next
                                            |> Maybe.map OneMore
                                            |> Maybe.withDefault Done

                                    else
                                        model.pendingFeed
                              }
                            , let
                                cmds =
                                    case next of
                                        Just nextUrl ->
                                            case polling of
                                                AutoRefresh ->
                                                    if
                                                        model.latestNumberedPage
                                                            /= Just nextUrl
                                                            && not
                                                                (model.gotPages
                                                                    |> Set.member nextUrl
                                                                )
                                                    then
                                                        -- Keep on loading since we are overdue by
                                                        -- more than one page. A rare case though!
                                                        [ getFeed
                                                            (Backfill model.latestNumberedPage)
                                                            nextUrl
                                                        ]

                                                    else
                                                        []

                                                Backfill dest ->
                                                    if dest == Just nextUrl then
                                                        []

                                                    else
                                                        [ getFeed polling nextUrl ]

                                                _ ->
                                                    []

                                        Nothing ->
                                            [ removeScrollEventListener () ]

                                cmds2 =
                                    if polling == Initial || polling == AutoRefresh then
                                        -- Set up next update.
                                        autoRefresh model.now events :: cmds

                                    else
                                        cmds

                                cmds3 =
                                    if entries |> List.any Event.isOngoing then
                                        -- Reset the clock to get one-second-precision time required by
                                        -- ongoing events.
                                        (Time.now |> Task.perform Tick) :: cmds2

                                    else
                                        cmds2
                              in
                              Cmd.batch cmds3
                            )

                        Err err ->
                            ( { model
                                | pendingFeed = Retry url
                                , errors = FeedHttpError url err :: model.errors
                              }
                            , Cmd.none
                            )
            in
            if not model2.initialized then
                -- Slide to current time once the first page is read, assuming that the first page
                -- is large enough to contain current time. If KemoV becomes big enough to fill up
                -- the first page with upcoming streams in the future, then we should revisit this!
                ( { model2 | initialized = True }, Cmd.batch [ cmd, slideViewportInto "now" ] )

            else
                ( model2, cmd )

        RetryGetFeed url ->
            update (GetFeed url)
                { model
                    | pendingFeed = Loading
                    , errors =
                        model.errors
                            |> List.filter
                                (\err ->
                                    case err of
                                        FeedHttpError _ _ ->
                                            False

                                        _ ->
                                            True
                                )
                }

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


getCopying : Cmd Msg
getCopying =
    Http.get { url = "COPYING", expect = Http.expectString AboutGotCopying }


autoRefresh : Time.Posix -> List Event -> Cmd Msg
autoRefresh now events =
    let
        interval =
            if
                events
                    |> List.any
                        (\e ->
                            Event.isOngoing e
                                || ((e.duration == Nothing)
                                        && (Time.posixToMillis e.time - Time.posixToMillis now)
                                        < (5 * 60 * 1000)
                                   )
                        )
            then
                -- Refresh more frequently if there's an ongoing or imminent stream.
                5 * 1000

            else
                60 * 1000
    in
    Process.sleep interval |> Task.perform (\() -> Refresh)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        subs =
            [ onKeyDown keyDecoder
            , Browser.Events.onClick (D.succeed CloseWidgets)
            ]

        subs2 =
            case model.pendingFeed of
                OneMore url ->
                    onScrollToBottom (\() -> GetFeed url) :: subs

                _ ->
                    subs
    in
    Sub.batch subs2


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
        [ lazy3 viewAboutDialog model.mode model.copying model.translations
        , div [ class "primary-window", ariaHidden <| model.mode /= None ]
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
            , header [ class "app-title", class "drawer-right" ]
                [ h1 [] [ text (T.title model.translations) ] ]
            , div [ id "drawer", class "drawer" ] [ viewDrawer model ]
            , div [ class "drawer-right" ] [ viewMain model, viewErrorLog model ]
            ]
        ]
    }


viewDrawer : Model -> Html Msg
viewDrawer model =
    menu
        [ class "filter-menu"
        , role "toolbar"
        , ariaOrientation "vertical"
        , ariaLabel (T.filterMenuLabel model.translations)
        ]
        [ li []
            [ button
                [ class "drawer-labelled-button"
                , class "filter-clear-button"
                , class "unstyle"
                , title <| T.clearFilter model.translations
                , disabled <| not (filterApplied model)
                , onClick ClearFilter
                , ariaLabelledby "filter-clear-button-label"
                ]
                -- Using `Html.Attributes.class` function here would cause an exception
                -- (in pure Elm, wow!) of setting getter-only property `className`.
                [ Icon.clear [ Svg.Attributes.class "drawer-icon" ]
                , span [ id "filter-clear-button-label", class "drawer-button-label" ]
                    [ text <| T.clearFilter model.translations ]
                ]
            ]
        , li [] <| viewSearch model
        , hr [] []
        , viewFeedFilter model
        , hr [] []
        , li []
            [ button
                [ id "about-button"
                , class "drawer-labelled-button"
                , class "about-button"
                , class "unstyle"
                , ariaControls "about"
                , ariaExpanded <|
                    case model.mode of
                        About _ ->
                            True

                        _ ->
                            False
                , ariaHaspopup "dialog"
                , ariaLabelledby "about-button-label"
                , Html.Events.stopPropagationOn "click" <|
                    D.succeed ( SetMode <| About AboutMain, True )
                ]
                [ Icon.about [ Svg.Attributes.class "drawer-icon" ]
                , span [ id "about-button-label", class "drawer-button-label" ]
                    [ text <| TAbout.title model.translations ]
                ]
            ]
        ]


filterApplied : Model -> Bool
filterApplied model =
    model.search /= "" || not (model.feeds |> List.all .checked)


viewSearch : Model -> List (Html Msg)
viewSearch model =
    [ label [ class "search-label" ]
        [ Icon.search
            [ Svg.Attributes.class "drawer-icon"
            , ariaLabel <| T.search model.translations
            ]
        , div [ class "search-container" ]
            [ input
                [ id "calendar-search"
                , type_ "search"
                , value model.search
                , list "searchlist"
                , ariaKeyshortcuts "S"
                , onInput SearchInput
                , onFocus (SearchFocus True)
                , onBlur (SearchFocus False)
                ]
                []
            ]
        ]
    , datalist [ id "searchlist" ]
        (model.events
            |> List.concatMap (\event -> searchTags event.name)
            |> Util.cardinalities
            |> DictUtil.groupKeysBy normalizeSearchTerm
            |> Dict.values
            |> List.filterMap
                (\pairs ->
                    pairs
                        -- Use the most common form among unnormalized terms.
                        |> List.Extra.maximumBy Tuple.second
                        -- This should never produce `Nothing` though.
                        |> Maybe.map
                            (\( tag, _ ) -> ( tag, pairs |> List.map Tuple.second |> List.sum ))
                )
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
    li [ class "feed-filter", ariaLabel (T.feedFilterLabel model.translations) ]
        [ ul []
            (model.feeds
                |> List.map
                    (\feed ->
                        let
                            pId =
                                "feed-" ++ feed.preset.id
                        in
                        li [ class "filter-item" ]
                            [ button
                                [ class "drawer-labelled-button"
                                , class "filter-button"
                                , class "unstyle"
                                , role "switch"
                                , title feed.preset.title
                                , onClick (ToggleFeedFilter feed.preset.id (not feed.checked))
                                , checked feed.checked
                                , ariaChecked feed.checked
                                , ariaLabelledby pId
                                ]
                                [ img
                                    [ class "avatar"
                                    , class "drawer-icon"
                                    , src feed.preset.icon
                                    , alt (T.avatarAlt model.translations)
                                    ]
                                    []
                                , span [ id pId, class "drawer-button-label" ]
                                    [ text feed.preset.title ]
                                ]
                            ]
                    )
            )
        ]


type TimelineItem
    = TimelineEvent ( Feed, Event.Event )
    | Now (List ( Feed, Event.Event ))


viewMain : Model -> Html Msg
viewMain model =
    let
        busy =
            model.pendingFeed == Loading
    in
    Keyed.node "main"
        [ role "feed", ariaBusy busy ]
        (let
            events =
                model.events
                    |> List.filterMap
                        (\event ->
                            model.feeds
                                |> List.Extra.find (\feed -> feed.preset.id == event.feed)
                                |> Maybe.map (\feed -> ( feed, event ))
                        )

            anyEventIsShown =
                events
                    |> List.any (\( feed, event ) -> eventIsShown model feed.checked event)

            ( ongoing, ( upcoming, past ) ) =
                let
                    ( og, other ) =
                        events |> List.partition (\( _, event ) -> Event.isOngoing event)
                in
                ( og
                , other
                    |> List.Extra.splitWhen
                        (\( _, event ) ->
                            Time.posixToMillis event.time <= Time.posixToMillis model.now
                        )
                    |> Maybe.withDefault ( other, [] )
                    |> Tuple.mapBoth (List.map TimelineEvent) (List.map TimelineEvent)
                )
         in
         ((upcoming ++ (Now ongoing :: past))
            |> Util.groupBy
                (\item ->
                    case item of
                        TimelineEvent ( _, event ) ->
                            NaiveDate.fromPosix model.timeZone event.time

                        Now _ ->
                            NaiveDate.fromPosix model.timeZone model.now
                )
            |> List.map (\( date, items ) -> viewKeyedDateSection model date items)
         )
            ++ [ ( "empty"
                 , div
                    [ class "empty-result"
                    , hidden
                        (busy || anyEventIsShown || model.pendingFeed /= Done)
                    ]
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
               , ( "loadMore"
                 , div [ id "feedBottom", class "load-more-feed" ] <|
                    case model.pendingFeed of
                        OneMore url ->
                            -- Scrolling to here will trigger loading of more items so there's
                            -- usually no need for a special button here, but there's this should be focusable in order to make it
                            -- keyboard-navigable.
                            [ button
                                [ class "load-more-feed-button"
                                , class "unstyle"
                                , ariaLabel <| T.loadMoreLabel model.translations
                                , onClick <| GetFeed url
                                ]
                                [ text <| T.loadMore model.translations ]
                            ]

                        Retry url ->
                            [ button
                                [ class "load-more-feed-button"
                                , class "unstyle"
                                , ariaLabel <| T.retryLoadingLabel model.translations
                                , onClick <| RetryGetFeed url
                                ]
                                [ text <| T.retryLoading model.translations ]
                            ]

                        Loading ->
                            [ text <| T.loading model.translations ]

                        Done ->
                            [ div []
                                [ p [] [ text <| T.noMoreItems model.translations ]
                                , p [ hidden <| filterApplied model ]
                                    [ text <| T.noMoreItemsGibberish model.translations ]
                                ]
                            ]
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
                                TimelineEvent ( feed, event ) ->
                                    eventIsShown model feed.checked event

                                Now _ ->
                                    True
                        )
                )
        ]
        [ header [ class "date-heading" ] [ h2 [] [ intlDate [] date ] ]
        , Keyed.ul [ class "timeline" ]
            (items
                |> List.map
                    (\item ->
                        case item of
                            TimelineEvent ( feed, event ) ->
                                viewKeyedEvent model feed event

                            Now ongoing_items ->
                                ( "now"
                                , let
                                    viewTime =
                                        intlTime [ class "flashing-time" ] model.now
                                  in
                                  if
                                    ongoing_items
                                        |> List.any
                                            (\( feed, event ) ->
                                                eventIsShown model feed.checked event
                                            )
                                  then
                                    section [ id "now", class "ongoing" ]
                                        [ header [ class "now" ]
                                            [ h2 []
                                                (T.ongoingCustom model.translations
                                                    text
                                                    viewTime
                                                )
                                            ]
                                        , Keyed.ul [ class "timeline" ]
                                            (ongoing_items
                                                |> List.map
                                                    (\( feed, event ) ->
                                                        viewKeyedEvent model feed event
                                                    )
                                            )
                                        ]

                                  else
                                    h2 [ id "now", class "now" ]
                                        [ h2 [] <|
                                            T.nowSeparatorCustom model.translations
                                                text
                                                viewTime
                                        ]
                                )
                    )
            )
        ]


viewKeyedEvent : Model -> Feed -> Event.Event -> ( String, Html Msg )
viewKeyedEvent model feed event =
    let
        eventId =
            "event-" ++ event.id

        headingId =
            eventId ++ "-heading"

        -- Construct ARIA description as an element rather than a single text,
        -- because the inner text of `<intl-time>` element is not accessible from Elm.
        descriptionId =
            eventId ++ "-description"

        eventHeader =
            let
                heading =
                    h3 [ id headingId ] [ text event.name ]

                headerContent =
                    case event.thumbnail of
                        Just thumb ->
                            [ heading
                            , div
                                [ class "event-thumbnail-container" ]
                                (let
                                    viewImg =
                                        img
                                            [ class "event-thumbnail"
                                            , loading "lazy"
                                            , src thumb
                                            , alt <| T.thumbnailAlt model.translations
                                            ]
                                            []
                                 in
                                 case event.duration of
                                    Just duration ->
                                        [ viewImg
                                        , time
                                            [ class "event-duration"
                                            , datetime <| Duration.toDatetime duration
                                            ]
                                            (Duration.render duration)
                                        ]

                                    Nothing ->
                                        if not event.upcoming then
                                            let
                                                duration =
                                                    Duration.fromMillis <|
                                                        Time.posixToMillis model.now
                                                            - Time.posixToMillis event.time
                                            in
                                            [ viewImg
                                            , time
                                                [ class "event-duration"
                                                , class "flashing-time"
                                                , datetime <| Duration.toDatetime duration
                                                ]
                                                (text "<" :: Duration.render duration)
                                            ]

                                        else
                                            [ viewImg ]
                                )
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

        eta =
            Duration.fromMillis <|
                (Time.posixToMillis event.time - Time.posixToMillis model.now)

        members =
            event.members
                |> List.filterMap
                    (\feedId ->
                        model.feeds
                            |> List.Extra.find (\f -> f.preset.id == feedId)
                    )

        memberPresets =
            members |> List.map .preset

        ( viewTimeInfo, description ) =
            let
                viewTime =
                    intlTime [] event.time
            in
            case event.duration of
                Just duration ->
                    let
                        viewDuration =
                            time [ datetime <| Duration.toDatetime duration ]
                                (Duration.render duration)
                    in
                    if event.live then
                        ( TEvent.startedAtCustom model.translations text viewTime
                        , TEventDescription.endedLiveCustom model.translations
                            text
                            (text <| T.members model.translations feed.preset memberPresets)
                            viewTime
                            viewDuration
                        )

                    else
                        ( TEvent.uploadedAtCustom model.translations text viewTime
                        , TEventDescription.videoCustom model.translations
                            text
                            (text <| T.members model.translations feed.preset memberPresets)
                            viewTime
                            viewDuration
                        )

                Nothing ->
                    if event.upcoming then
                        let
                            viewDuration =
                                T.viewDuration model.translations eta

                            viewStartsIn =
                                if Duration.isNegative eta then
                                    TEvent.dueAgoCustom model.translations text <|
                                        T.viewDuration model.translations (Duration.negate eta)

                                else
                                    TEvent.startsInCustom model.translations text viewDuration
                        in
                        ( TEvent.timeWithEtaCustom model.translations
                            (text >> List.singleton)
                            [ viewTime ]
                            viewStartsIn
                            |> List.concat
                        , TEventDescription.scheduledLiveCustom model.translations
                            text
                            (text <| T.members model.translations feed.preset memberPresets)
                            viewDuration
                        )

                    else
                        let
                            viewDuration =
                                T.viewDuration model.translations <| Duration.negate eta

                            viewStartedAgo =
                                TEvent.startedAgoCustom model.translations text viewDuration
                        in
                        ( TEvent.timeWithEtaCustom model.translations
                            (text >> List.singleton)
                            [ viewTime ]
                            viewStartedAgo
                            |> List.concat
                        , TEventDescription.ongoingLiveCustom model.translations
                            text
                            (text <| T.members model.translations feed.preset memberPresets)
                            viewDuration
                        )
    in
    ( eventId
    , li
        [ hidden <| not <| eventIsShown model feed.checked event ]
        [ article
            [ class "event"
            , ariaLabelledby headingId
            , ariaDescribedby descriptionId
            ]
            (div [ class "event-time" ] viewTimeInfo
                :: eventHeader
                :: ul [ class "event-members" ]
                    (viewEventMember True feed :: (members |> List.map (viewEventMember False)))
                :: div [ id descriptionId, hidden True ] description
                :: (if model.features.copy || model.features.share then
                        List.singleton <|
                            lazy4 viewEventPopup
                                model.features
                                model.translations
                                model.activePopup
                                event

                    else
                        []
                   )
            )
        ]
    )


viewEventPopup : Features -> Translations -> Maybe String -> Event.Event -> Html Msg
viewEventPopup features translations activePopup event =
    let
        popupId =
            "popup-" ++ event.id

        expanded =
            activePopup == Just event.id
    in
    div
        [ class "popup-container" ]
        [ button
            [ class "popup-toggle"
            , class "unstyle"
            , ariaHaspopup "menu"
            , ariaControls popupId
            , ariaExpanded expanded
            , -- Call `stopPropagation` to prevent `CloseWidgets` message to be sent.
              Html.Events.stopPropagationOn "click" <|
                D.succeed
                    ( if expanded then
                        ClosePopup

                      else
                        OpenPopup event.id
                    , True
                    )
            , ariaLabel <| TShare.share translations
            ]
            -- TODO: Add an icon.
            [ text "…" ]
        , menu
            [ id popupId
            , classList [ ( "popup", True ), ( "expanded", expanded ) ]
            , ariaLabel <| TShare.share translations
            ]
            -- TODO: Add icons to list items too.
            (let
                items =
                    []

                items2 =
                    if features.share then
                        li [] (viewShareEvent translations event) :: items

                    else
                        items

                items3 =
                    if features.copy then
                        li [] (viewCopyEventTimestamp translations event)
                            :: li [] (viewCopyEvent translations event)
                            :: items2

                    else
                        items2
             in
             items3
            )
        ]


viewCopyEvent : Translations -> Event.Event -> List (Html Msg)
viewCopyEvent translations event =
    let
        copyText =
            event.link
                |> Maybe.map (\link -> event.name ++ "\n" ++ link)
                |> Maybe.withDefault event.name
    in
    [ button
        [ class "unstyle", onClick <| Copy copyText ]
        [ text <| TShare.copyTitleAndUrl translations ]
    ]


viewCopyEventTimestamp : Translations -> Event.Event -> List (Html Msg)
viewCopyEventTimestamp translations event =
    [ button
        [ class "unstyle"
        , onClick <| Copy (String.fromInt (Time.posixToMillis event.time // 1000))
        ]
        [ text <| TShare.copyTimestamp translations ]
    ]


viewShareEvent : Translations -> Event.Event -> List (Html Msg)
viewShareEvent translations event =
    [ button
        [ class "unstyle", onClick <| Share event.name event.link ]
        [ text <| TShare.shareVia translations ]
    ]


eventIsShown : Model -> Bool -> Event.Event -> Bool
eventIsShown model feedChecked event =
    (feedChecked
        || -- Check that any of the members' feed is checked.
           (model.feeds
                |> List.any (\feed -> feed.checked && (event.members |> List.member feed.preset.id))
           )
    )
        && searchMatches model event


searchMatches : Model -> Event.Event -> Bool
searchMatches model event =
    String.isEmpty model.search
        || (event.name |> normalizeSearchTerm |> String.contains (normalizeSearchTerm model.search))


normalizeSearchTerm : String -> String
normalizeSearchTerm text =
    text |> String.toUpper |> String.replace "＃" "#"


viewEventMember : Bool -> Feed -> Html Msg
viewEventMember isAuthor feed =
    li [ class "event-member" ]
        [ a
            (href feed.alternate
                :: (if isAuthor then
                        [ rel "author" ]

                    else
                        []
                   )
            )
            [ img [ class "avatar", src feed.preset.icon, alt feed.preset.title ] [] ]
        ]


viewErrorLog : Model -> Html Msg
viewErrorLog model =
    ul
        [ class "error-log"
        , role "log"
        , ariaLive "assertive"
        , ariaLabel <| TError.error model.translations
        , hidden (List.isEmpty model.errors)
        ]
        (model.errors
            |> List.Extra.indexedFoldl
                (\i err acc -> li [] (viewError model i err) :: acc)
                []
        )


viewError : Model -> Int -> Error -> List (Html Msg)
viewError model errIdx err =
    case err of
        TranslationsHttpError lang e ->
            let
                url =
                    translationsUrl lang
            in
            [ text "Error retrieving <"
            , a [ href url ] [ text url ]
            , text <| ">: " ++ httpErrorToString e
            , button
                [ class "dismiss-error"
                , class "unstyle"
                , onClick <| RetryGetTranslations lang errIdx
                ]
                [ text "Retry" ]
            ]

        FeedHttpError url e ->
            TError.httpCustom model.translations
                text
                (a [ href url ] [ text url ])
                (text <| httpErrorToString e)
                ++ [ button
                        [ class "dismiss-error"
                        , class "unstyle"
                        , onClick <| DismissError errIdx
                        ]
                        [ text <| TError.dismiss model.translations ]
                   ]

        Unexpected msg ->
            -- I'd prefer the application to simply crash in the event of a programming error which
            -- cannot be caught by the compiler like this, but Elm doesn't allow it.
            [ text <| TError.unexpected model.translations msg ]


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


viewAboutDialog : Mode -> Maybe (Result Http.Error (Html Msg)) -> Translations -> Html Msg
viewAboutDialog mode copying translations =
    dialog
        [ id "about"
        , class "about"
        , class "modal-backdrop"
        , role "dialog"
        , ariaModal True
        , ariaLabelledby "about-heading"
        ]
        [ -- The purposes of this `div` are:
          -- 1. To prevent the click event from firinng on the backdrop
          -- 2. To make polyfill styling easier
          div [ class "modal", Html.Events.stopPropagationOn "click" <| D.succeed ( NoOp, True ) ]
            [ header [ class "dialog-title-bar" ]
                [ button
                    [ class "dialog-title-bar-button"
                    , class "modal-back-button"
                    , class "unstyle"
                    , disabled <| mode == About AboutMain
                    , ariaLabel <| T.goBackTo translations <| TAbout.title translations
                    , onClick AboutBackToMain
                    ]
                    [ Icon.backButton ]
                , h2 [ id "about-heading", class "dialog-title" ]
                    [ text <|
                        case mode of
                            About AboutCopying ->
                                "COPYING"

                            _ ->
                                TAbout.title translations
                    ]
                , button
                    [ id "about-close-button"
                    , class "dialog-title-bar-button"
                    , class "unstyle"
                    , ariaLabel <| T.closeDialog translations
                    , onClick <| SetMode None
                    ]
                    [ Icon.closeDialog ]
                ]
            , div
                [ class "dialog-content", hidden <| mode /= About AboutMain ]
                (viewAboutDialogMain translations)
            , viewAboutDialogCopying copying
                [ class "dialog-content", hidden <| mode /= About AboutCopying ]
            ]
        ]


viewAboutDialogMain : Translations -> List (Html Msg)
viewAboutDialogMain translations =
    [ p [] [ text <| TAbout.introduction translations ]
    , p [] [ text <| TAbout.aboutAccuracy translations ]
    , p []
        (TAbout.disclaimerCustom translations
            text
            (a [ href "https://github.com/U-cauda-elongata/calendar" ]
                [ text <| TAbout.gitHubRepository translations ]
            )
        )
    , p [] [ text <| TAbout.rights translations ]
    , p [] [ text <| TAbout.appeal translations ]
    , h3 [] [ text <| TAbout.licenseHeading translations ]
    , p []
        (TAbout.licenseBodyCustom translations text <|
            a
                [ href "COPYING"
                , Html.Events.preventDefaultOn "click" <| D.succeed ( AboutOpenCopying, True )
                ]
                [ text "COPYING" ]
        )
    , h3 [] [ text <| TAbout.links translations ]
    , ul []
        [ li []
            [ a [ href "https://github.com/U-cauda-elongata/calendar" ]
                [ Icon.gitHub [ Svg.Attributes.class "social-icon", ariaHidden True ]
                , text "GitHub"
                ]
            ]
        ]
    ]


viewAboutDialogCopying : Maybe (Result Http.Error (Html Msg)) -> List (Attribute Msg) -> Html Msg
viewAboutDialogCopying copying attrs =
    case copying of
        Nothing ->
            div (class "copying-loading" :: attrs) [ p [] [ text "Loading…" ] ]

        Just (Ok html) ->
            div attrs [ html ]

        Just (Err err) ->
            div (class "copying-error" :: attrs)
                [ h3 [] [ text "Error" ]
                , p [] [ text <| httpErrorToString err ]
                , button [ onClick AboutRetryGetCopying ] [ text "Retry" ]
                ]
