module Calendar.Event exposing (Event, feedDecoder)

import Calendar.Feeds exposing (Feed)
import Dict exposing (Dict)
import Iso8601
import Parser
import Time
import Xml.Decode as XD


type alias Event =
    { name : String
    , updated : Time.Posix
    , link : Maybe String
    , thumbnail : Maybe String
    , members : List Feed
    }


feedDecoder : Dict String Feed -> XD.Decoder (List Event)
feedDecoder feeds =
    XD.path [ "entry" ] (XD.list (entryDecoder feeds))


entryDecoder : Dict String Feed -> XD.Decoder Event
entryDecoder feeds =
    XD.map5 Event
        (XD.path [ "title" ] (XD.single XD.string))
        (XD.path [ "updated" ] (XD.single XD.string)
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
            (XD.succeed (\v -> v |> Maybe.map (extractMembers feeds) |> Maybe.withDefault []))
        )


extractMembers : Dict String Feed -> String -> List Feed
extractMembers feeds description =
    feeds
        |> Dict.foldr
            (\_ ->
                \feed ->
                    \members ->
                        if String.contains ("@" ++ feed.title) description then
                            feed :: members

                        else
                            members
            )
            []
