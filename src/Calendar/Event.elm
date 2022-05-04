module Calendar.Event exposing (Event, feedDecoder)

import Iso8601
import Parser
import Time
import Xml.Decode as XD


type alias Event =
    { feed : String
    , name : String
    , updated : Time.Posix
    , link : Maybe String
    , thumbnail : Maybe String
    }


entryDecoder : String -> XD.Decoder Event
entryDecoder url =
    XD.map5 Event
        (XD.succeed url)
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
        (XD.possiblePath [ "media:group", "media:thumbnail" ] (XD.single (XD.stringAttr "url")) (XD.succeed identity))


feedDecoder : String -> XD.Decoder (List Event)
feedDecoder url =
    XD.path [ "entry" ] (XD.list (entryDecoder url))
