module Status exposing (Status(..), view)

import Filter exposing (Feed)
import Html exposing (..)
import Html.Attributes exposing (..)
import I18Next exposing (Translations)
import Translations.Status as T


type Status
    = ClearFilter
    | ClearFeedFilter
    | ShowFeed Feed
    | HideFeed Feed
    | HideOtherFeeds Feed


view : Translations -> Status -> List (Html msg)
view translations status =
    case status of
        ClearFilter ->
            [ text <| T.clearFilter translations ]

        ClearFeedFilter ->
            [ text <| T.clearFeedFilter translations ]

        ShowFeed feed ->
            T.showingFeedCustom translations text <|
                span [ lang feed.preset.lang ] [ text feed.preset.title ]

        HideFeed feed ->
            T.hidingFeedCustom translations text <|
                span [ lang feed.preset.lang ] [ text feed.preset.title ]

        HideOtherFeeds feed ->
            T.showingOnlyCustom translations text <|
                span [ lang feed.preset.lang ] [ text feed.preset.title ]
