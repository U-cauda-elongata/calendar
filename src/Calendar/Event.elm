module Calendar.Event exposing (Event, feedDecoder)

import Calendar.Feeds as Feeds
import Iso8601
import List.Extra
import Parser
import Time
import Xml.Decode as XD


type alias Event =
    { name : String
    , time : Time.Posix
    , link : Maybe String
    , thumbnail : Maybe String
    , members : List Int
    }


feedDecoder : List Feeds.Metadata -> Feeds.Metadata -> XD.Decoder (List Event)
feedDecoder feeds feed =
    XD.path [ "entry" ] (XD.list (entryDecoder feeds feed))


entryDecoder : List Feeds.Metadata -> Feeds.Metadata -> XD.Decoder Event
entryDecoder feeds meta =
    XD.map5 Event
        (XD.path [ "title" ] (XD.single XD.string))
        (XD.path [ "published" ] (XD.single XD.string)
            |> XD.andThen
                (\date ->
                    case Iso8601.toTime date of
                        Ok time ->
                            XD.succeed time

                        Err deadEnds ->
                            XD.fail (Parser.deadEndsToString deadEnds)
                )
        )
        (XD.possiblePath [ "link" ] (XD.single (XD.stringAttr "href")) (XD.succeed identity))
        (XD.possiblePath [ "media:group", "media:thumbnail" ]
            (XD.single (XD.stringAttr "url"))
            (XD.succeed identity)
        )
        (XD.possiblePath [ "media:group", "media:description" ]
            (XD.single XD.string)
            (XD.succeed (\v -> v |> Maybe.map (extractMembers feeds meta) |> Maybe.withDefault []))
        )


extractMembers : List Feeds.Metadata -> Feeds.Metadata -> String -> List Int
extractMembers feeds thisFeed description =
    feeds
        |> List.Extra.indexedFoldr
            (\i feed members ->
                if feed /= thisFeed && String.contains ("@" ++ feed.title) description then
                    i :: members

                else
                    members
            )
            []
