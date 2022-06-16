port module Main exposing (main)

import Attributes exposing (..)
import Browser exposing (Document)
import Browser.Dom as Dom
import Browser.Events exposing (Visibility(..), onKeyDown, onVisibilityChange)
import Browser.Navigation as Nav
import Dict
import Duration
import Elements exposing (..)
import Event exposing (Event)
import Feed
import Filter exposing (Feed, Filter)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onClick, onDoubleClick, onFocus, onInput)
import Html.Keyed as Keyed
import Html.Lazy exposing (..)
import Http
import I18Next exposing (Translations, translationsDecoder)
import Icon
import Json.Decode as D
import List.Extra as List
import Markdown
import NaiveDate exposing (NaiveDate)
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
import TranslationsExt as T
import Url exposing (Url)
import Url.Builder
import Url.Parser
import Url.Parser.Query as Query
import Util
import Util.Dict as Dict


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
    , tz : Time.Zone

    -- Fields used by `view`:
    , now : Time.Posix
    , pendingFeed : PendingFeed
    , errors : List Error
    , -- An ephemeral message to be announced by screen readers.
      status : Maybe String

    -- Widgets:
    , drawerExpanded : Bool
    , searchFocused : Bool
    , activePopup : Maybe String

    -- Filter:
    , filter : Filter
    , searchSuggestions : List String

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

        filter =
            Filter query.q (flags.feeds |> List.map (Feed True "") |> applyQueryToFeeds query)

        baseUrl =
            { url | query = Nothing }
    in
    ( { initialized = False
      , latestNumberedPage = Nothing
      , gotPages = Set.empty
      , visibility = Visible
      , key = key
      , url = baseUrl
      , features = flags.features
      , translations = initialTranslations
      , tz = Time.utc
      , now = Time.millisToPosix 0
      , pendingFeed = Loading
      , status = Nothing
      , errors = []
      , drawerExpanded = False
      , searchFocused = False
      , activePopup = Nothing
      , filter = filter
      , searchSuggestions = []
      , mode = None
      , copying = Nothing
      , events = []
      }
    , Cmd.batch
        [ Time.here |> Task.perform SetTimeZone
        , getTranslations <| selectLanguage flags.languages
        , Time.now |> Task.perform Tick
        , replaceQuery key baseUrl filter
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
        (Query.custom "empty" <| not << List.isEmpty)


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
            ( model
            , if Filter.isActive model.filter then
                pushQuery model.key model.url <| Filter.clear model.filter

              else
                Cmd.none
            )

        ClearFeedFilter ->
            ( model
            , if model.filter.feeds |> List.all .checked then
                Cmd.none

              else
                pushQuery model.key model.url <| Filter.clearFeeds model.filter
            )

        SearchInput q ->
            let
                filter =
                    model.filter
            in
            ( model, replaceQuery model.key model.url { filter | q = q } )

        ToggleFeedFilter i ->
            ( model, pushQuery model.key model.url <| Filter.toggleFeed i model.filter )

        HideOtherFeeds i ->
            let
                filter =
                    model.filter

                ( feeds, updated ) =
                    model.filter.feeds
                        |> List.indexedFoldl
                            (\j feed ( acc, u ) ->
                                let
                                    checked =
                                        i == j
                                in
                                if checked == feed.checked then
                                    ( feed :: acc, u )

                                else
                                    ( { feed | checked = checked } :: acc, True )
                            )
                            ( [], False )
            in
            ( model
            , if updated then
                pushQuery model.key model.url { filter | feeds = feeds }

              else
                Cmd.none
            )

        SetTimeZone tz ->
            ( { model | tz = tz }, Cmd.none )

        Tick now ->
            ( { model | now = now }
            , let
                ms =
                    Time.posixToMillis now
              in
              if
                (model.visibility == Visible)
                    -- Avoid issuing further task if it is likely that another task is running.
                    && ((abs <| Time.posixToMillis model.now - ms) > 100)
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
                        interval - (ms |> remainderBy interval)
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
            ( { model | errors = model.errors |> List.removeAt errIdx }, Cmd.none )

        RetryGetTranslations lang errIdx ->
            ( { model | errors = model.errors |> List.removeAt errIdx }, getTranslations lang )

        KeyDown key ->
            let
                setStatus status ( m, cmd ) =
                    ( { m | status = Just status }, Cmd.batch [ clearStatus, cmd ] )

                toggle i =
                    let
                        ret =
                            update (ToggleFeedFilter i) model
                    in
                    model.filter.feeds
                        |> List.getAt i
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
                            update (HideOtherFeeds i) model
                    in
                    model.filter.feeds
                        |> List.getAt i
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
                        [ Dom.focus nowSectionId |> Task.attempt handleDomResult
                        , slideViewportInto nowSectionId
                        ]
                    )

                "S" ->
                    ( model, Dom.focus searchInputId |> Task.attempt handleDomResult )

                "S-S" ->
                    ( model, Dom.focus searchInputId |> Task.attempt handleDomResult )

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
                        |> Tuple.mapSecond
                            (\cmd ->
                                Cmd.batch
                                    [ cmd
                                    , Dom.blur searchInputId |> Task.attempt handleDomResult
                                    ]
                            )

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
            ( model
            , Cmd.batch
                [ Dom.blur searchInputId |> Task.attempt handleDomResult
                , pushQuery model.key model.url model.filter
                ]
            )

        SearchClear ->
            let
                filter =
                    model.filter
            in
            ( model
            , Cmd.batch
                (let
                    cmds =
                        [ Dom.blur searchInputId |> Task.attempt handleDomResult ]
                 in
                 if String.isEmpty filter.q then
                    cmds

                 else
                    pushQuery model.key model.url { filter | q = "" } :: cmds
                )
            )

        SearchFocus value ->
            ( { model | searchFocused = value }
            , -- Prevent `.drawer` to scroll into the search input before the transition completes.
              Dom.setViewportOf drawerId 0 0 |> Task.attempt handleDomResult
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
                        [ close aboutDialogId
                        , Dom.focus aboutButtonId |> Task.attempt handleDomResult
                        ]

                ( Help, Help ) ->
                    Cmd.none

                _ ->
                    Cmd.batch
                        [ case model.mode of
                            None ->
                                Cmd.none

                            About _ ->
                                close aboutDialogId

                            Help ->
                                close helpDialogId
                        , case mode of
                            None ->
                                Cmd.none

                            About _ ->
                                Cmd.batch
                                    [ showModal aboutDialogId
                                    , Dom.focus aboutCloseButtonId |> Task.attempt handleDomResult
                                    ]

                            Help ->
                                Cmd.batch
                                    [ showModal helpDialogId
                                    , Dom.focus helpCloseButtonId
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
                                filter =
                                    model.filter

                                feeds =
                                    meta
                                        |> List.foldl
                                            (\m ->
                                                List.map
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
                                            model.filter.feeds

                                events =
                                    (case polling of
                                        AutoRefresh ->
                                            Util.mergeBy .id model.events entries

                                        Backfill _ ->
                                            Util.mergeBy .id model.events entries

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
                                | filter = { filter | feeds = feeds }
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
            if model2.initialized then
                ( model2, cmd )

            else
                -- Slide to current time once the first page is read, assuming that the first page
                -- is large enough to contain current time. If KemoV becomes big enough to fill up
                -- the first page with upcoming streams in the future, then we should revisit this!
                ( { model2 | initialized = True }
                , Cmd.batch [ cmd, slideViewportInto nowSectionId ]
                )

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
            ( { model | filter = Filter query.q (model.filter.feeds |> applyQueryToFeeds query) }
            , Cmd.none
            )


pushQuery : Nav.Key -> Url -> Filter -> Cmd msg
pushQuery key url filter =
    case Filter.toQueryString filter of
        "" ->
            Nav.pushUrl key <| Url.toString url

        q ->
            Nav.pushUrl key q


replaceQuery : Nav.Key -> Url -> Filter -> Cmd msg
replaceQuery key url filter =
    case Filter.toQueryString filter of
        "" ->
            Nav.replaceUrl key <| Url.toString url

        q ->
            Nav.replaceUrl key q


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
        |> Dict.groupKeysBy normalizeSearchTerm
        |> Dict.values
        |> List.filterMap
            (\pairs ->
                pairs
                    -- Use the most common form among unnormalized terms.
                    |> List.maximumBy Tuple.second
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
            , Browser.Events.onClick <| D.succeed ClosePopup
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
                                                    - Time.posixToMillis model.now
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


drawerId : String
drawerId =
    "drawer"


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
                    , ( "filter-active", Filter.isActive model.filter )
                    ]
                , ariaLabelledby hamburgerLabelId
                , ariaDescribedby hamburgerDescriptionId
                , onClick <| HamburgerChecked <| not drawerExpanded
                ]
                [ Icon.hamburger [ ariaLabelledby hamburgerLabelId ] ]
            , header [ class "app-title" ]
                [ h1 [] [ text <| T.title model.translations ] ]
            , div [ id drawerId, class "drawer" ]
                [ lazy5 viewDrawer
                    model.translations
                    drawerExpanded
                    model.mode
                    model.searchSuggestions
                    model.filter
                ]
            , div [ class "main-container" ]
                [ lazy8 viewMain
                    model.translations
                    model.features
                    model.tz
                    model.now
                    model.activePopup
                    model.pendingFeed
                    model.filter
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


aboutButtonId : String
aboutButtonId =
    "about-button"


hamburgerLabelId : String
hamburgerLabelId =
    "hamburger-label"


hamburgerDescriptionId : String
hamburgerDescriptionId =
    "hamburger-description"


viewDrawer : Translations -> Bool -> Mode -> List String -> Filter -> Html Msg
viewDrawer translations expanded mode searchSuggestions filter =
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
            , ariaDescribedby hamburgerDescriptionId
            , onClick <| HamburgerChecked <| not expanded
            ]
            [ span
                [ id hamburgerLabelId
                , class "hamburger-label"
                , class "drawer-button-label"
                ]
                [ text <|
                    if expanded then
                        T.collapseMenu translations

                    else
                        T.expandMenu translations
                ]
            , span [ id hamburgerDescriptionId, hidden True ]
                [ text <| T.hamburgerDescription translations ]
            ]
        , let
            labelId =
                "filter-clear-button-label"
          in
          li []
            [ button
                [ class "drawer-labelled-button"
                , class "filter-clear-button"
                , class "unstyle"
                , title <| T.clearFilter translations
                , disabled <| not <| Filter.isActive filter
                , onClick ClearFilter
                , ariaKeyshortcuts "Shift+0"
                , ariaLabelledby labelId
                ]
                -- Using `Html.Attributes.class` function here would cause an exception
                -- (in pure Elm, wow!) of setting getter-only property `className`.
                [ Icon.clear [ Svg.Attributes.class "drawer-icon", ariaLabelledby labelId ]
                , span [ id labelId, class "drawer-button-label" ]
                    [ text <| T.clearFilter translations ]
                ]
            ]
        , li [] <| viewSearch translations searchSuggestions filter.q
        , hr [] []
        , viewFeedFilter translations filter.feeds
        , hr [] []
        , let
            labelId =
                "about-button-label"

            labelText =
                TAbout.title translations
          in
          li []
            [ button
                [ id aboutButtonId
                , class "drawer-labelled-button"
                , class "about-button"
                , class "unstyle"
                , title labelText
                , ariaControls aboutDialogId
                , ariaExpanded <|
                    case mode of
                        About _ ->
                            True

                        _ ->
                            False
                , ariaHaspopup "dialog"
                , ariaLabelledby labelId
                , Html.Events.stopPropagationOn "click" <|
                    D.succeed ( SetMode <| About AboutMain, True )
                ]
                [ Icon.about [ Svg.Attributes.class "drawer-icon", ariaLabelledby labelId ]
                , span [ id labelId, class "drawer-button-label" ]
                    [ text <| labelText ]
                ]
            ]
        ]


searchInputId : String
searchInputId =
    "calendar-search"


viewSearch : Translations -> List String -> String -> List (Html Msg)
viewSearch translations suggestions q =
    let
        labelText =
            T.search translations

        datalistId =
            "searchlist"
    in
    [ label [ class "search-label", title labelText ]
        [ Icon.search [ Svg.Attributes.class "drawer-icon", ariaLabel labelText ]
        , div [ class "search-container" ]
            [ input
                [ id searchInputId
                , type_ "search"
                , value q
                , list datalistId
                , ariaKeyshortcuts "S"
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
    , datalist [ id datalistId ] (suggestions |> List.map (\term -> option [ value term ] []))
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
                                    , alt feed.preset.title
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
    -> Time.Zone
    -> Time.Posix
    -> Maybe String
    -> PendingFeed
    -> Filter
    -> List Event
    -> Html Msg
viewMain translations features tz now activePopup pendingFeed filter events =
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
                            filter.feeds
                                |> List.find (\feed -> feed.preset.id == event.feed)
                                |> Maybe.map (\feed -> ( feed, event ))
                        )

            anyEventIsShown =
                feedAndEvents
                    |> List.any
                        (\( feed, event ) ->
                            eventIsShown filter feed.checked event
                        )

            ( ongoing, ( upcoming, past ) ) =
                let
                    ( og, other ) =
                        feedAndEvents |> List.partition (\( _, event ) -> Event.isOngoing event)
                in
                ( og
                , other
                    |> List.splitWhen
                        (\( _, event ) -> Time.posixToMillis event.time <= Time.posixToMillis now)
                    |> Maybe.withDefault ( other, [] )
                    |> Tuple.mapBoth (List.map TimelineEvent) (List.map TimelineEvent)
                )
         in
         ((upcoming ++ (Now ongoing :: past))
            |> Util.groupBy
                (\item ->
                    case item of
                        TimelineEvent ( _, event ) ->
                            NaiveDate.fromPosix tz event.time

                        Now _ ->
                            NaiveDate.fromPosix tz now
                )
            |> List.map
                (\( date, items ) ->
                    viewKeyedDateSection translations features now activePopup filter date items
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
                            -- usually no need for a special button here, but there's this should be
                            -- focusable in order to make it keyboard-navigable.
                            [ button
                                [ class "load-more-feed-button"
                                , class "fine-pointer"
                                , class "unstyle"
                                , ariaLabel <| T.loadMoreLabel translations
                                , onClick <| GetFeed url
                                ]
                                [ text <| T.clickToLoadMore translations ]
                            , button
                                [ class "load-more-feed-button"
                                , class "no-fine-pointer"
                                , class "unstyle"
                                , ariaLabel <| T.loadMoreLabel translations
                                , onClick <| GetFeed url
                                ]
                                [ text <| T.tapToLoadMore translations ]
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
                                , p [ hidden <| Filter.isActive filter ]
                                    [ text <| T.noMoreItemsGibberish translations ]
                                ]
                            ]
                 )
               ]
        )


nowSectionId : String
nowSectionId =
    "now"


viewKeyedDateSection :
    Translations
    -> Features
    -> Time.Posix
    -> Maybe String
    -> Filter
    -> NaiveDate
    -> List TimelineItem
    -> ( String, Html Msg )
viewKeyedDateSection translations features now activePopup filter date items =
    ( NaiveDate.toIso8601 date
    , section
        [ hidden <|
            not
                (items
                    |> List.any
                        (\item ->
                            case item of
                                TimelineEvent ( feed, event ) ->
                                    eventIsShown filter feed.checked event

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
                                viewKeyedEvent translations features now activePopup filter
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
                                                eventIsShown filter feed.checked event
                                            )
                                  then
                                    section [ id nowSectionId, class "ongoing", tabindex -1 ]
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
                                    h2 [ id nowSectionId, class "now", tabindex -1 ]
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
    -> Filter
    -> Feed
    -> Event
    -> ( String, Html Msg )
viewKeyedEvent translations features now activePopup filter feed event =
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
                                        if event.upcoming then
                                            [ viewImg ]

                                        else
                                            let
                                                duration =
                                                    Duration.subPosix now event.time
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
            Duration.subPosix event.time now

        members =
            event.members
                |> List.filterMap
                    (\feedId -> filter.feeds |> List.find (\f -> f.preset.id == feedId))

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
        [ hidden <| not <| eventIsShown filter feed.checked event ]
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
            , -- Call `stopPropagation` to prevent `CloseWidgets` message from being sent.
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


eventIsShown : Filter -> Bool -> Event -> Bool
eventIsShown filter feedChecked event =
    (feedChecked
        || -- Check that any of the members' feed is checked.
           (filter.feeds
                |> List.any (\feed -> feed.checked && (event.members |> List.member feed.preset.id))
           )
    )
        && searchMatches filter.q event


searchMatches : String -> Event -> Bool
searchMatches q event =
    let
        name =
            event.name |> normalizeSearchTerm
    in
    String.isEmpty q
        || (normalizeSearchTerm q
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
        (errors |> List.indexedFoldl (\i err acc -> li [] (viewError translations i err) :: acc) [])


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


aboutCloseButtonId : String
aboutCloseButtonId =
    "about-close-button"


aboutDialogId : String
aboutDialogId =
    "about"


viewAboutDialog : Mode -> Maybe (Result Http.Error (Html Msg)) -> Translations -> Html Msg
viewAboutDialog mode copying translations =
    let
        headingId =
            "about-heading"
    in
    dialog
        [ id aboutDialogId
        , class "about"
        , class "modal-backdrop"
        , role "dialog"
        , ariaModal True
        , ariaLabelledby headingId
        ]
        [ -- The purposes of this `div` are:
          -- 1. To prevent the click event from firing on the backdrop
          -- 2. To make polyfill styling easier
          div [ class "modal" ]
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
                , h2 [ id headingId, class "dialog-title" ]
                    [ text <|
                        case mode of
                            About AboutCopying ->
                                "COPYING"

                            _ ->
                                TAbout.title translations
                    ]
                , button
                    [ id aboutCloseButtonId
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
            ]
        , button
            [ class "modal-backdrop-button"
            , class "unstyle"
            , ariaLabel <| T.srCloseDialog translations
            , onClick <| SetMode None
            ]
            []
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
                [ Icon.gitHub [ Svg.Attributes.class "social-icon", ariaLabel "GitHub" ]
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


helpDialogId : String
helpDialogId =
    "help"


helpCloseButtonId : String
helpCloseButtonId =
    "help-close-button"


viewHelpDialog : Translations -> Mode -> Html Msg
viewHelpDialog translations mode =
    let
        headingId =
            "kdb-help-heading"
    in
    dialog
        [ id helpDialogId
        , class "modal-backdrop"
        , role "dialog"
        , ariaModal True
        , ariaLabelledby headingId
        ]
        [ div [ class "modal", Html.Events.stopPropagationOn "click" <| D.succeed ( NoOp, True ) ]
            [ header [ class "dialog-title-bar" ]
                [ h2 [ id headingId, class "dialog-title" ]
                    [ text <| THelp.title translations ]
                , button
                    [ id helpCloseButtonId
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
            ]
        , button
            [ class "modal-backdrop-button"
            , class "unstyle"
            , ariaLabel <| T.srCloseDialog translations
            , onClick <| SetMode None
            ]
            []
        ]
