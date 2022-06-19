module Query exposing (Query, applyToFeeds, parseUrl)

import Filter exposing (Feed)
import Set exposing (Set)
import Url exposing (Url)
import Url.Parser
import Url.Parser.Query as Query exposing (Parser)


type alias Query =
    { q : String
    , feed : Set String
    , empty : Bool
    }


parseUrl : Url -> Query
parseUrl url =
    { url | path = "/" }
        |> Url.Parser.parse (Url.Parser.query parser)
        |> Maybe.withDefault (Query "" Set.empty False)


parser : Parser Query
parser =
    Query.map3 Query
        (Query.string "q" |> Query.map (Maybe.withDefault ""))
        (Query.custom "feed" Set.fromList)
        (Query.custom "empty" <| not << List.isEmpty)


applyToFeeds : Query -> List Feed -> List Feed
applyToFeeds query feeds =
    let
        showAll =
            Set.isEmpty query.feed && not query.empty
    in
    feeds
        |> List.map
            (\feed -> { feed | checked = showAll || (query.feed |> Set.member feed.preset.id) })
