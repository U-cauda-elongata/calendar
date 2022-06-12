port module Main exposing (main)

import Browser exposing (Document)
import Browser.Dom as Dom
import Browser.Events exposing (Visibility(..), onKeyDown, onVisibilityChange)
import Browser.Navigation as Nav
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
import Html.Events exposing (onBlur, onCheck, onClick, onDoubleClick, onFocus, onInput)
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
import Translations.Help as THelp
import Translations.Share as TShare
import Translations.Status as TStatus
import Url exposing (Url)
import Url.Builder
import Url.Parser
import Url.Parser.Query as Query


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



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



-- MODEL


type alias Model =
    { -- FIelds not used by `view`:
      -- Set this to `True` once the first feed request completes, in order to prevent subsequent
      -- requests from causing the initial `slideViewportInto` to be called again.
      initialized : Bool
    , latestNumberedPage : Maybe String
    , gotPages : Set String
    , visibility : Visibility

    -- Fields that are set by `init` or `Cmd`s issued by it and never changes:
    , key : Nav.Key
    , -- Base URL of the app.
      url : Url
    , features : Features
    , translations : Translations

    -- Fields used by `view`:
    , time : { now : Time.Posix, zone : Time.Zone }
    , pendingFeed : PendingFeed
    , errors : List Error
    , -- An ephemeral message to be announced by screen readers.
      status : Maybe String

    -- Widgets:
    , drawerExpanded : Bool
    , searchFocused : Bool
    , activePopup : Maybe String

    -- Filter:
    , search : String
    , searchSuggestions : List String
    , feeds : List Feed

    -- Modal dialogs:
    , mode : Mode
    , copying : Maybe (Result Http.Error (Html Msg))

    -- Main:
    , events : List Event
    }


type alias Features =
    { copy : Bool
    , share : Bool
    }


type alias Feed =
    { checked : Bool
    , alternate : String
    , preset : Feed.Preset
    }


type PendingFeed
    = OneMore String
    | Retry String
    | Loading
    | Done


type Mode
    = None
    | About AboutView
    | Help


type AboutView
    = AboutMain
    | AboutCopying


type Error
    = FeedHttpError String Http.Error
    | TranslationsHttpError String Http.Error
    | Unexpected String


type alias Flags =
    { features : Features
    , languages : List String
    , feeds : List Feed.Preset
    }


type alias Query =
    { q : String
    , feed : Set String
    , empty : Bool
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        query =
            parseQuery url
    in
    ( { initialized = False
      , latestNumberedPage = Nothing
      , gotPages = Set.empty
      , visibility = Visible
      , key = key
      , url = { url | query = Nothing }
      , features = flags.features
      , translations = initialTranslations
      , time = { now = Time.millisToPosix 0, zone = Time.utc }
      , pendingFeed = Loading
      , status = Nothing
      , errors = []
      , drawerExpanded = False
      , searchFocused = False
      , activePopup = Nothing
      , search = query.q
      , searchSuggestions = []
      , feeds = flags.feeds |> List.map (Feed True "") |> applyQueryToFeeds query
      , mode = None
      , copying = Nothing
      , events = []
      }
    , Cmd.batch
        [ Time.here |> Task.perform SetTimeZone
        , getTranslations <| selectLanguage flags.languages
        , Time.now |> Task.perform Tick
        , getFeed Initial "latest.json"
        ]
    )


parseQuery : Url -> Query
parseQuery url =
    { url | path = "/" }
        |> Url.Parser.parse (Url.Parser.query queryParser)
        |> Maybe.withDefault (Query "" Set.empty False)


queryParser : Query.Parser Query
queryParser =
    Query.map3 Query
        (Query.string "q" |> Query.map (Maybe.withDefault ""))
        (Query.custom "feed" Set.fromList)
        (Query.string "empty" |> Query.map (Maybe.map (always True) >> Maybe.withDefault False))


applyQueryToFeeds : Query -> List Feed -> List Feed
applyQueryToFeeds query feeds =
    let
        showAll =
            Set.isEmpty query.feed && not query.empty
    in
    feeds
        |> List.map
            (\feed -> { feed | checked = showAll || (query.feed |> Set.member feed.preset.id) })


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


type PollingKind
    = Initial
    | Manual
    | AutoRefresh
    | Backfill (Maybe String)


type Msg
    = HamburgerChecked Bool
    | ClearFilter
    | ClearFeedFilter
    | SearchInput String
    | ToggleFeedFilter Int
    | HideOtherFeeds Int
    | KeyDown String
    | VisibilityChanged Visibility
    | SearchConfirm
    | SearchClear
    | SearchFocus Bool
    | GetFeed String
    | Refresh
    | SetMode Mode
    | CloseWidgets
    | ClearStatus
    | AboutBackToMain
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
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HamburgerChecked value ->
            ( { model | drawerExpanded = value }, Cmd.none )

        ClearFilter ->
            let
                new =
                    { model
                        | search = ""
                        , feeds = model.feeds |> List.map (\feed -> { feed | checked = True })
                    }
            in
            ( new, pushQuery new )

        ClearFeedFilter ->
            let
                new =
                    { model | feeds = model.feeds |> List.map (\feed -> { feed | checked = True }) }
            in
            ( new, pushQuery new )

        SearchInput search ->
            let
                new =
                    { model | search = search }
            in
            ( new, replaceQuery new )

        ToggleFeedFilter i ->
            let
                new =
                    { model
                        | feeds =
                            model.feeds
                                |> List.Extra.updateAt i
                                    (\feed -> { feed | checked = not feed.checked })
                    }
            in
            ( new, pushQuery new )

        HideOtherFeeds i ->
            let
                new =
                    { model
                        | feeds =
                            model.feeds |> List.indexedMap (\j feed -> { feed | checked = i == j })
                    }
            in
            ( new, pushQuery new )

        SetTimeZone timeZone ->
            let
                time =
                    model.time
            in
            ( { model | time = { time | zone = timeZone } }, Cmd.none )

        Tick now ->
            let
                time =
                    model.time
            in
            ( { model | time = { time | now = now } }
            , let
                ms =
                    Time.posixToMillis now
              in
              if
                (model.visibility == Visible)
                    -- Avoid issuing further task if it is likely that another task is running.
                    && ((abs <| Time.posixToMillis model.time.now - ms) > 100)
              then
                let
                    interval =
                        if model.events |> List.any Event.isOngoing then
                            -- An ongoing event need to show the duration elapsed since the start
                            -- time in one-second precision.
                            1000

                        else
                            60000

                    nextTick =
                        interval - (ms |> modBy interval)
                in
                Process.sleep (nextTick |> toFloat)
                    |> Task.andThen (\() -> Time.now)
                    |> Task.perform Tick

              else
                Cmd.none
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
            let
                setStatus status ( m, cmd ) =
                    ( { m | status = Just status }, Cmd.batch [ clearStatus, cmd ] )

                toggle i =
                    let
                        ret =
                            update (ToggleFeedFilter i) model
                    in
                    model.feeds
                        |> List.Extra.getAt i
                        |> Maybe.map
                            (\feed ->
                                ret
                                    |> setStatus
                                        (if feed.checked then
                                            TStatus.showingFeed model.translations feed.preset.title

                                         else
                                            TStatus.hidingFeed model.translations feed.preset.title
                                        )
                            )
                        |> Maybe.withDefault ret

                hideOthers i =
                    let
                        ret =
                            update (HideOtherFeeds 0) model
                    in
                    model.feeds
                        |> List.Extra.getAt i
                        |> Maybe.map
                            (\feed ->
                                ret
                                    |> setStatus
                                        (TStatus.showingOnly model.translations feed.preset.title)
                            )
                        |> Maybe.withDefault ret
            in
            case key of
                "N" ->
                    ( model
                    , Cmd.batch
                        [ Dom.focus "now" |> Task.attempt handleDomResult
                        , slideViewportInto "now"
                        ]
                    )

                "S" ->
                    ( model, focusSearch )

                "S-S" ->
                    ( model, focusSearch )

                "X" ->
                    update (HamburgerChecked <| not model.drawerExpanded) model

                "0" ->
                    update ClearFeedFilter model
                        |> setStatus (TStatus.clearFeedFilter model.translations)

                "S-0" ->
                    update ClearFilter model
                        |> setStatus (TStatus.clearFilter model.translations)

                "1" ->
                    toggle 0

                "S-1" ->
                    hideOthers 0

                "2" ->
                    toggle 1

                "S-2" ->
                    hideOthers 1

                "3" ->
                    toggle 2

                "S-3" ->
                    hideOthers 2

                "4" ->
                    toggle 3

                "S-4" ->
                    hideOthers 3

                "5" ->
                    toggle 4

                "S-5" ->
                    hideOthers 4

                "6" ->
                    toggle 5

                "S-6" ->
                    hideOthers 5

                "7" ->
                    toggle 6

                "S-7" ->
                    hideOthers 6

                "8" ->
                    toggle 7

                "S-8" ->
                    hideOthers 7

                "9" ->
                    toggle 8

                "S-9" ->
                    hideOthers 8

                "?" ->
                    update (SetMode Help) model

                "S-?" ->
                    update (SetMode Help) model

                "ESCAPE" ->
                    update CloseWidgets model
                        |> Tuple.mapSecond (\cmd -> Cmd.batch [ cmd, blurSearch ])

                _ ->
                    ( model, Cmd.none )

        VisibilityChanged visibility ->
            ( { model | visibility = visibility }
            , if visibility == Visible then
                Cmd.batch
                    [ getFeed AutoRefresh "latest.json"
                    , Time.now |> Task.perform Tick
                    ]

              else
                Cmd.none
            )

        SearchConfirm ->
            ( model, Cmd.batch [ blurSearch, pushQuery model ] )

        SearchClear ->
            ( { model | search = "" }, blurSearch )

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
                ( None, None ) ->
                    Cmd.none

                ( About _, About _ ) ->
                    Cmd.none

                ( About _, None ) ->
                    Cmd.batch
                        [ close "about"
                        , Dom.focus "about-button" |> Task.attempt handleDomResult
                        ]

                ( Help, Help ) ->
                    Cmd.none

                _ ->
                    Cmd.batch
                        [ case model.mode of
                            None ->
                                Cmd.none

                            About _ ->
                                close "about"

                            Help ->
                                close "help"
                        , case mode of
                            None ->
                                Cmd.none

                            About _ ->
                                Cmd.batch
                                    [ showModal "about"
                                    , Dom.focus "about-close-button"
                                        |> Task.attempt handleDomResult
                                    ]

                            Help ->
                                Cmd.batch
                                    [ showModal "help"
                                    , Dom.focus "help-close-button"
                                        |> Task.attempt handleDomResult
                                    ]
                        ]
            )

        CloseWidgets ->
            update (SetMode None) { model | activePopup = Nothing }

        ClearStatus ->
            ( { model | status = Nothing }, Cmd.none )

        AboutBackToMain ->
            ( { model | mode = About AboutMain }, Cmd.none )

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
                                , searchSuggestions = searchTagsFromEvents events
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
                                    if
                                        (entries |> List.any Event.isOngoing)
                                            && not (model.events |> List.any Event.isOngoing)
                                    then
                                        -- Reset the clock to get one-second-precision time required
                                        -- by ongoing events.
                                        (Time.now |> Task.perform Tick) :: cmds

                                    else
                                        cmds
                              in
                              Cmd.batch cmds2
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

        LinkClicked (Browser.Internal url) ->
            case url.path |> Util.stripPrefix model.url.path of
                Just "COPYING" ->
                    ( { model | mode = About AboutCopying }
                    , case model.copying of
                        Just _ ->
                            Cmd.none

                        Nothing ->
                            getCopying
                    )

                _ ->
                    ( model, Nav.load <| Url.toString url )

        LinkClicked (Browser.External href) ->
            ( model, Nav.load href )

        UrlChanged url ->
            let
                query =
                    parseQuery url
            in
            ( { model | search = query.q, feeds = model.feeds |> applyQueryToFeeds query }
            , Cmd.none
            )


pushQuery : Model -> Cmd Msg
pushQuery model =
    case toQueryString model.search model.feeds of
        "" ->
            Nav.pushUrl model.key <| Url.toString model.url

        q ->
            Nav.pushUrl model.key q


replaceQuery : Model -> Cmd Msg
replaceQuery model =
    case toQueryString model.search model.feeds of
        "" ->
            Nav.replaceUrl model.key <| Url.toString model.url

        q ->
            Nav.replaceUrl model.key q


toQueryString : String -> List Feed -> String
toQueryString q feeds =
    Url.Builder.toQuery <|
        let
            queries =
                case
                    feeds
                        |> List.foldr
                            (\feed ( qs, uncheckedAny ) ->
                                ( if feed.checked then
                                    Url.Builder.string "feed" feed.preset.id :: qs

                                  else
                                    qs
                                , uncheckedAny || not feed.checked
                                )
                            )
                            ( [], False )
                of
                    ( [], _ ) ->
                        [ Url.Builder.string "empty" "" ]

                    ( qs, True ) ->
                        qs

                    ( _, False ) ->
                        []
        in
        if q == "" then
            queries

        else
            Url.Builder.string "q" q :: queries


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
            ReportError <| Unexpected <| "Node not found: " ++ id


clearStatus : Cmd Msg
clearStatus =
    Process.sleep 1000 |> Task.perform (always ClearStatus)


getCopying : Cmd Msg
getCopying =
    Http.get { url = "COPYING", expect = Http.expectString AboutGotCopying }


searchTagsFromEvents : List Event -> List String
searchTagsFromEvents events =
    events
        |> List.concatMap (searchTags << .name)
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
        |> List.sortBy (Tuple.second >> negate)
        |> List.map Tuple.first


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        subs =
            [ onKeyDown <| D.map KeyDown keyDecoder
            , Browser.Events.onClick <| D.succeed CloseWidgets
            , onVisibilityChange VisibilityChanged
            ]

        subs2 =
            case model.pendingFeed of
                OneMore url ->
                    onScrollToBottom (\() -> GetFeed url) :: subs

                _ ->
                    subs

        subs3 =
            if model.visibility == Visible then
                let
                    interval =
                        if
                            model.events
                                |> List.any
                                    (\e ->
                                        Event.isOngoing e
                                            || ((e.duration == Nothing)
                                                    && Time.posixToMillis e.time
                                                    - Time.posixToMillis model.time.now
                                                    < (5 * 60 * 1000)
                                               )
                                    )
                        then
                            -- Refresh more frequently if there's an ongoing or imminent stream.
                            5 * 1000

                        else
                            60 * 1000
                in
                Time.every interval (always Refresh) :: subs2

            else
                subs2
    in
    Sub.batch subs3


keyDecoder : D.Decoder String
keyDecoder =
    D.field "code" D.string
        |> D.andThen
            (\code ->
                -- Special-casing for digit keys because we want to match on combinations like
                -- `<S-1>`, which maps to `key` value of `!` on US QWERTY layout for example.
                case code |> Util.stripPrefix "Digit" of
                    Just n ->
                        D.succeed n

                    Nothing ->
                        D.field "key" D.string |> D.map String.toUpper
            )
        |> D.andThen (appendModifier "shiftKey" "S-")
        |> D.andThen (appendModifier "metaKey" "M-")
        |> D.andThen (appendModifier "ctrlKey" "C-")
        |> D.andThen (appendModifier "altKey" "A-")


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
        , lazy2 viewHelpDialog model.translations model.mode
        , let
            drawerExpanded =
                model.drawerExpanded || model.searchFocused
          in
          div
            [ class "primary-window", ariaHidden <| model.mode /= None ]
            [ button
                [ class "hamburger"
                , class "unstyle"
                , classList
                    [ ( "checked", drawerExpanded )
                    , ( "filter-active", filterApplied model.search model.feeds )
                    ]
                , ariaHidden True
                , onClick <| HamburgerChecked <| not drawerExpanded
                ]
                [ Icon.hamburger ]
            , header [ class "app-title" ]
                [ h1 [] [ text <| T.title model.translations ] ]
            , div [ id "drawer", class "drawer" ]
                [ lazy6 viewDrawer
                    model.translations
                    drawerExpanded
                    model.mode
                    model.searchSuggestions
                    model.search
                    model.feeds
                ]
            , div [ class "main-container" ]
                [ lazy8 viewMain
                    model.translations
                    model.features
                    model.time
                    model.activePopup
                    model.pendingFeed
                    model.search
                    model.feeds
                    model.events
                , lazy2 viewErrorLog model.translations model.errors
                ]
            ]
        , div
            [ role "status"
            , model.status |> Maybe.map ariaLabel |> Maybe.withDefault (hidden True)
            ]
            []
        ]
    }


viewDrawer : Translations -> Bool -> Mode -> List String -> String -> List Feed -> Html Msg
viewDrawer translations expanded mode searchSuggestions search feeds =
    menu
        [ class "drawer-menu"
        , class "unstyle"
        , role "toolbar"
        , ariaOrientation "vertical"
        , ariaLabel <| T.filterMenuLabel translations
        ]
        [ button
            [ class "drawer-labelled-button"
            , class "unstyle"
            , onClick <| HamburgerChecked <| not expanded
            , ariaHidden True
            ]
            [ span
                [ class "hamburger-label"
                , class "drawer-button-label"
                ]
                [ text <| T.collapseMenu translations ]
            ]
        , li []
            [ button
                [ class "drawer-labelled-button"
                , class "filter-clear-button"
                , class "unstyle"
                , title <| T.clearFilter translations
                , disabled <| not <| filterApplied search feeds
                , onClick ClearFilter
                , ariaKeyshortcuts "Shift+0"
                , ariaLabelledby "filter-clear-button-label"
                ]
                -- Using `Html.Attributes.class` function here would cause an exception
                -- (in pure Elm, wow!) of setting getter-only property `className`.
                [ Icon.clear [ Svg.Attributes.class "drawer-icon" ]
                , span [ id "filter-clear-button-label", class "drawer-button-label" ]
                    [ text <| T.clearFilter translations ]
                ]
            ]
        , li [] <| viewSearch translations searchSuggestions search
        , hr [] []
        , viewFeedFilter translations feeds
        , hr [] []
        , let
            labelText =
                TAbout.title translations
          in
          li []
            [ button
                [ id "about-button"
                , class "drawer-labelled-button"
                , class "about-button"
                , class "unstyle"
                , title labelText
                , ariaControls "about"
                , ariaExpanded <|
                    case mode of
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
                    [ text <| labelText ]
                ]
            ]
        ]


filterApplied : String -> List Feed -> Bool
filterApplied search feeds =
    search /= "" || not (feeds |> List.all .checked)


viewSearch : Translations -> List String -> String -> List (Html Msg)
viewSearch translations suggestions search =
    let
        labelText =
            T.search translations
    in
    [ label [ class "search-label", title labelText ]
        [ Icon.search [ Svg.Attributes.class "drawer-icon", ariaHidden True ]
        , div [ class "search-container" ]
            [ input
                [ id "calendar-search"
                , type_ "search"
                , value search
                , list "searchlist"
                , ariaKeyshortcuts "S"
                , ariaLabel labelText
                , onInput SearchInput
                , Html.Events.stopPropagationOn "keydown"
                    (keyDecoder
                        |> D.map
                            (\key ->
                                case key of
                                    "ENTER" ->
                                        SearchConfirm

                                    "ESCAPE" ->
                                        SearchClear

                                    _ ->
                                        NoOp
                            )
                        |> D.map (\msg -> ( msg, True ))
                    )
                , onFocus <| SearchFocus True
                , onBlur <| SearchFocus False
                ]
                []
            ]
        ]
    , datalist [ id "searchlist" ] (suggestions |> List.map (\term -> option [ value term ] []))
    ]


viewFeedFilter : Translations -> List Feed -> Html Msg
viewFeedFilter translations feeds =
    li [ class "feed-filter" ]
        [ ul [ ariaLabel <| T.feedFilterLabel translations ]
            (feeds
                |> List.indexedMap
                    (\i feed ->
                        let
                            labelId =
                                "feed-" ++ feed.preset.id
                        in
                        li []
                            [ button
                                [ class "drawer-labelled-button"
                                , class "filter-button"
                                , class "unstyle"
                                , role "switch"
                                , title feed.preset.title
                                , onClick <| ToggleFeedFilter i
                                , onDoubleClick <| HideOtherFeeds i
                                , checked feed.checked
                                , ariaChecked feed.checked
                                , ariaKeyshortcuts <| String.fromInt <| i + 1
                                , ariaLabelledby labelId
                                ]
                                [ img
                                    [ class "avatar"
                                    , class "drawer-icon"
                                    , src feed.preset.icon
                                    , alt <| T.avatarAlt translations
                                    ]
                                    []
                                , span [ id labelId, class "drawer-button-label" ]
                                    [ text feed.preset.title ]
                                ]
                            ]
                    )
            )
        ]


type TimelineItem
    = TimelineEvent ( Feed, Event.Event )
    | Now (List ( Feed, Event.Event ))


viewMain :
    Translations
    -> Features
    -> { now : Time.Posix, zone : Time.Zone }
    -> Maybe String
    -> PendingFeed
    -> String
    -> List Feed
    -> List Event
    -> Html Msg
viewMain translations features time activePopup pendingFeed search feeds events =
    let
        busy =
            pendingFeed == Loading
    in
    Keyed.node "main"
        [ role "feed", ariaBusy busy ]
        (let
            feedAndEvents =
                events
                    |> List.filterMap
                        (\event ->
                            feeds
                                |> List.Extra.find (\feed -> feed.preset.id == event.feed)
                                |> Maybe.map (\feed -> ( feed, event ))
                        )

            anyEventIsShown =
                feedAndEvents
                    |> List.any
                        (\( feed, event ) ->
                            eventIsShown search feeds feed.checked event
                        )

            ( ongoing, ( upcoming, past ) ) =
                let
                    ( og, other ) =
                        feedAndEvents |> List.partition (\( _, event ) -> Event.isOngoing event)
                in
                ( og
                , other
                    |> List.Extra.splitWhen
                        (\( _, event ) ->
                            Time.posixToMillis event.time <= Time.posixToMillis time.now
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
                            NaiveDate.fromPosix time.zone event.time

                        Now _ ->
                            NaiveDate.fromPosix time.zone time.now
                )
            |> List.map
                (\( date, items ) ->
                    viewKeyedDateSection
                        translations
                        features
                        time.now
                        activePopup
                        search
                        feeds
                        date
                        items
                )
         )
            ++ [ ( "empty"
                 , div
                    [ class "empty-result"
                    , hidden <| busy || anyEventIsShown
                    ]
                    (let
                        pre =
                            p [] [ text <| T.emptyResultPre translations ]

                        post =
                            p [ hidden <| pendingFeed /= Done ]
                                [ text <| T.emptyResultPost translations ]
                     in
                     case T.emptyResultKidding translations of
                        "" ->
                            [ pre, post ]

                        mid ->
                            [ pre
                            , p [ hidden <| pendingFeed /= Done ] [ del [] [ text mid ] ]
                            , post
                            ]
                    )
                 )
               , -- This is used to visually put the following items at the bottom.
                 -- I suspect there is a better way though.
                 ( "padding", div [ class "flex-padding" ] [] )
               , ( "loadMore"
                 , div [ id "feedBottom", class "load-more-feed" ] <|
                    case pendingFeed of
                        OneMore url ->
                            -- Scrolling to here will trigger loading of more items so there's
                            -- usually no need for a special button here, but there's this should be focusable in order to make it
                            -- keyboard-navigable.
                            [ button
                                [ class "load-more-feed-button"
                                , class "unstyle"
                                , ariaLabel <| T.loadMoreLabel translations
                                , onClick <| GetFeed url
                                ]
                                [ text <| T.loadMore translations ]
                            ]

                        Retry url ->
                            [ button
                                [ class "load-more-feed-button"
                                , class "unstyle"
                                , ariaLabel <| T.retryLoadingLabel translations
                                , onClick <| RetryGetFeed url
                                ]
                                [ text <| T.retryLoading translations ]
                            ]

                        Loading ->
                            [ text <| T.loading translations ]

                        Done ->
                            [ div []
                                [ p [] [ text <| T.noMoreItems translations ]
                                , p [ hidden <| filterApplied search feeds ]
                                    [ text <| T.noMoreItemsGibberish translations ]
                                ]
                            ]
                 )
               ]
        )


viewKeyedDateSection :
    Translations
    -> Features
    -> Time.Posix
    -> Maybe String
    -> String
    -> List Feed
    -> NaiveDate
    -> List TimelineItem
    -> ( String, Html Msg )
viewKeyedDateSection translations features now activePopup search feeds date items =
    ( NaiveDate.toIso8601 date
    , section
        [ hidden <|
            not
                (items
                    |> List.any
                        (\item ->
                            case item of
                                TimelineEvent ( feed, event ) ->
                                    eventIsShown search feeds feed.checked event

                                Now _ ->
                                    True
                        )
                )
        ]
        [ header [ class "date-heading" ] [ h2 [] [ intlDate [] date ] ]
        , Keyed.ul [ class "timeline", class "unstyle" ]
            (items
                |> List.map
                    (\item ->
                        let
                            partialViewKeyedEvent =
                                viewKeyedEvent
                                    translations
                                    features
                                    now
                                    activePopup
                                    search
                                    feeds
                        in
                        case item of
                            TimelineEvent ( feed, event ) ->
                                partialViewKeyedEvent feed event

                            Now ongoing_items ->
                                ( "now"
                                , let
                                    viewTime =
                                        intlTime [ class "flashing-time" ] now
                                  in
                                  if
                                    ongoing_items
                                        |> List.any
                                            (\( feed, event ) ->
                                                eventIsShown
                                                    search
                                                    feeds
                                                    feed.checked
                                                    event
                                            )
                                  then
                                    section [ id "now", class "ongoing", tabindex -1 ]
                                        [ header [ class "now" ]
                                            [ h2 []
                                                (T.ongoingCustom translations
                                                    text
                                                    viewTime
                                                )
                                            ]
                                        , Keyed.ul [ class "timeline", class "unstyle" ]
                                            (ongoing_items
                                                |> List.map
                                                    (\( feed, event ) ->
                                                        partialViewKeyedEvent feed event
                                                    )
                                            )
                                        ]

                                  else
                                    h2 [ id "now", class "now", tabindex -1 ]
                                        [ h2 [] <|
                                            T.nowSeparatorCustom translations
                                                text
                                                viewTime
                                        ]
                                )
                    )
            )
        ]
    )


viewKeyedEvent :
    Translations
    -> Features
    -> Time.Posix
    -> Maybe String
    -> String
    -> List Feed
    -> Feed
    -> Event
    -> ( String, Html Msg )
viewKeyedEvent translations features now activePopup search feeds feed event =
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
                                            , alt <| T.thumbnailAlt translations
                                            ]
                                            []
                                 in
                                 case event.duration of
                                    Just duration ->
                                        [ viewImg
                                        , time
                                            [ class "event-duration"
                                            , datetime <| Duration.toDatetime duration
                                            , role "time"
                                            ]
                                            (Duration.render duration)
                                        ]

                                    Nothing ->
                                        if not event.upcoming then
                                            let
                                                duration =
                                                    Duration.fromMillis <|
                                                        Time.posixToMillis now
                                                            - Time.posixToMillis event.time
                                            in
                                            [ viewImg
                                            , time
                                                [ class "event-duration"
                                                , class "flashing-time"
                                                , datetime <| Duration.toDatetime duration
                                                , role "timer"
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
                        header []
                            [ a [ class "event-header-grid", href link, ariaLabelledby headingId ]
                                headerContent
                            ]
                    )
                |> Maybe.withDefault
                    (header [ class "event-header-grid", ariaLabelledby headingId ]
                        headerContent
                    )

        eta =
            Duration.fromMillis <|
                (Time.posixToMillis event.time - Time.posixToMillis now)

        members =
            event.members
                |> List.filterMap
                    (\feedId -> feeds |> List.Extra.find (\f -> f.preset.id == feedId))

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
                            time [ datetime <| Duration.toDatetime duration, role "time" ]
                                (Duration.render duration)
                    in
                    if event.live then
                        ( TEvent.startedAtCustom translations text viewTime
                        , TEventDescription.endedLiveCustom translations
                            text
                            (text <| T.members translations feed.preset memberPresets)
                            viewTime
                            viewDuration
                        )

                    else
                        ( TEvent.uploadedAtCustom translations text viewTime
                        , TEventDescription.videoCustom translations
                            text
                            (text <| T.members translations feed.preset memberPresets)
                            viewTime
                            viewDuration
                        )

                Nothing ->
                    if event.upcoming then
                        let
                            viewDuration =
                                T.viewDuration translations eta

                            viewStartsIn =
                                if Duration.isNegative eta then
                                    TEvent.dueAgoCustom translations text <|
                                        T.viewDuration translations (Duration.negate eta)

                                else
                                    TEvent.startsInCustom translations text viewDuration
                        in
                        ( TEvent.timeWithEtaCustom translations
                            (text >> List.singleton)
                            [ viewTime ]
                            viewStartsIn
                            |> List.concat
                        , TEventDescription.scheduledLiveCustom translations
                            text
                            (text <| T.members translations feed.preset memberPresets)
                            viewDuration
                        )

                    else
                        let
                            viewDuration =
                                T.viewDuration translations <| Duration.negate eta

                            viewStartedAgo =
                                TEvent.startedAgoCustom translations text viewDuration
                        in
                        ( TEvent.timeWithEtaCustom translations
                            (text >> List.singleton)
                            [ viewTime ]
                            viewStartedAgo
                            |> List.concat
                        , TEventDescription.ongoingLiveCustom translations
                            text
                            (text <| T.members translations feed.preset memberPresets)
                            viewDuration
                        )
    in
    ( eventId
    , li
        [ hidden <| not <| eventIsShown search feeds feed.checked event ]
        [ article
            [ class "event"
            , ariaLabelledby headingId
            , ariaDescribedby descriptionId
            ]
            (div [ class "event-padding" ] [ div [] viewTimeInfo, eventHeader ]
                :: ul [ class "event-members" ]
                    (viewEventMember True feed :: (members |> List.map (viewEventMember False)))
                :: div [ id descriptionId, hidden True ] description
                :: (if features.copy || features.share then
                        List.singleton <|
                            lazy4 viewEventPopup
                                features
                                translations
                                (activePopup == Just event.id)
                                event

                    else
                        []
                   )
            )
        ]
    )


viewEventPopup : Features -> Translations -> Bool -> Event.Event -> Html Msg
viewEventPopup features translations expanded event =
    let
        popupId =
            "popup-" ++ event.id
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
            [ id popupId, class "popup", class "unstyle", ariaLabel <| TShare.share translations ]
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
        , onClick <| Copy <| String.fromInt <| Time.posixToMillis event.time // 1000
        ]
        [ text <| TShare.copyTimestamp translations ]
    ]


viewShareEvent : Translations -> Event.Event -> List (Html Msg)
viewShareEvent translations event =
    [ button
        [ class "unstyle", onClick <| Share event.name event.link ]
        [ text <| TShare.shareVia translations ]
    ]


eventIsShown : String -> List Feed -> Bool -> Event -> Bool
eventIsShown search feeds feedChecked event =
    (feedChecked
        || -- Check that any of the members' feed is checked.
           (feeds
                |> List.any (\feed -> feed.checked && (event.members |> List.member feed.preset.id))
           )
    )
        && searchMatches search event


searchMatches : String -> Event -> Bool
searchMatches search event =
    let
        name =
            event.name |> normalizeSearchTerm
    in
    String.isEmpty search
        || (normalizeSearchTerm search
                |> String.words
                |> List.all (\term -> name |> String.contains term)
           )


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
            [ img
                [ class "avatar"
                , src feed.preset.icon
                , alt feed.preset.title
                , width 60
                , height 60
                ]
                []
            ]
        ]


viewErrorLog : Translations -> List Error -> Html Msg
viewErrorLog translations errors =
    ul
        [ class "error-log"
        , class "unstyle"
        , role "log"
        , ariaLive "assertive"
        , ariaLabel <| TError.error translations
        , hidden <| List.isEmpty errors
        ]
        (errors
            |> List.Extra.indexedFoldl
                (\i err acc -> li [] (viewError translations i err) :: acc)
                []
        )


viewError : Translations -> Int -> Error -> List (Html Msg)
viewError translations errIdx err =
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
            TError.httpCustom translations
                text
                (a [ href url ] [ text url ])
                (text <| httpErrorToString e)
                ++ [ button
                        [ class "dismiss-error"
                        , class "unstyle"
                        , onClick <| DismissError errIdx
                        ]
                        [ text <| TError.dismiss translations ]
                   ]

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
                [ class "dialog-content", lang "en", hidden <| mode /= About AboutCopying ]
            , button
                [ class "dialog-sr-only-button"
                , class "unstyle"
                , ariaLabel <| T.srCloseDialog translations
                , onClick <| SetMode None
                ]
                []
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
    , p [] (TAbout.licenseBodyCustom translations text <| a [ href "COPYING" ] [ text "COPYING" ])
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


viewHelpDialog : Translations -> Mode -> Html Msg
viewHelpDialog translations mode =
    dialog
        [ id "help"
        , class "modal-backdrop"
        , role "dialog"
        , ariaModal True
        , ariaLabelledby "kdb-help-heading"
        ]
        [ div [ class "modal", Html.Events.stopPropagationOn "click" <| D.succeed ( NoOp, True ) ]
            [ header [ class "dialog-title-bar" ]
                [ h2 [ id "help-heading", class "dialog-title" ]
                    [ text <| THelp.title translations ]
                , button
                    [ id "help-close-button"
                    , class "dialog-title-bar-button"
                    , class "unstyle"
                    , ariaLabel <| T.closeDialog translations
                    , onClick <| SetMode None
                    ]
                    [ Icon.closeDialog ]
                ]
            , div
                [ class "dialog-content", hidden <| mode /= Help ]
                [ dl [ class "kbd-help-dl" ]
                    [ dt [] [ kbd [] [ text "n" ] ]
                    , dd [] [ text <| THelp.kbdN translations ]
                    , dt [] [ kbd [] [ text "s" ] ]
                    , dd [] [ text <| THelp.kbdS translations ]
                    , dt [] [ kbd [] [ text "x" ] ]
                    , dd [] [ text <| THelp.kbdX translations ]
                    , dt [] [ kbd [] [ text "N" ] ]
                    , dd [] [ text <| THelp.kbdDigit translations ]
                    , dt [] [ kbd [] [ kbd [] [ text "Shift" ], text "+", kbd [] [ text "N" ] ] ]
                    , dd [] [ text <| THelp.kbdSDigit translations ]
                    , dt [] [ kbd [] [ text "0" ] ]
                    , dd [] [ text <| THelp.kbd0 translations ]
                    , dt [] [ kbd [] [ kbd [] [ text "Shift" ], text "+", kbd [] [ text "0" ] ] ]
                    , dd [] [ text <| THelp.kbdS0 translations ]
                    , dt [] [ kbd [] [ text "?" ] ]
                    , dd [] [ text <| THelp.kbdQuestion translations ]
                    ]
                ]
            , button
                [ class "dialog-sr-only-button"
                , class "unstyle"
                , ariaLabel <| T.srCloseDialog translations
                , onClick <| SetMode None
                ]
                []
            ]
        ]
