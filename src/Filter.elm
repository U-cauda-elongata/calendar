module Filter exposing (Feed, Filter, clear, clearFeeds, isActive, toQueryString, toggleFeed)

import Feed
import List.Extra as List
import Url.Builder


type alias Feed =
    { checked : Bool
    , alternate : String
    , preset : Feed.Preset
    }


type alias Filter =
    { q : String
    , feeds : List Feed
    }


isActive : Filter -> Bool
isActive { q, feeds } =
    not <| String.isEmpty q && (feeds |> List.all .checked)


clear : Filter -> Filter
clear filter =
    { filter | q = "" } |> clearFeeds


clearFeeds : Filter -> Filter
clearFeeds filter =
    { filter | feeds = filter.feeds |> List.map (\feed -> { feed | checked = True }) }


toggleFeed : Int -> Filter -> Filter
toggleFeed i filter =
    { filter
        | feeds =
            filter.feeds
                |> List.updateAt i
                    (\feed -> { feed | checked = not feed.checked })
    }


toQueryString : Filter -> String
toQueryString { q, feeds } =
    Url.Builder.toQuery <|
        let
            queries =
                case
                    feeds
                        |> List.foldr
                            (\feed ( qs, checkedAll ) ->
                                ( if feed.checked then
                                    Url.Builder.string "feed" feed.preset.id :: qs

                                  else
                                    qs
                                , checkedAll && feed.checked
                                )
                            )
                            ( [], True )
                of
                    ( [], _ ) ->
                        [ Url.Builder.string "empty" "" ]

                    ( _, True ) ->
                        []

                    ( qs, False ) ->
                        qs
        in
        if q == "" then
            queries

        else
            Url.Builder.string "q" q :: queries
