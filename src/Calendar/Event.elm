module Calendar.Event exposing (Event, feedDecoder)

import Calendar.Feeds as Feeds
import Dict exposing (Dict)
import Json.Decode as D
import Time


type alias Event =
    { name : String
    , time : Time.Posix
    , link : Maybe String
    , thumbnail : Maybe String
    , members : List String
    }


type alias Entry =
    { name : String
    , time : Int
    , link : Maybe String
    , thumbnail : Maybe String
    , description : Maybe String
    }


feedDecoder : Dict String Feeds.Metadata -> D.Decoder (Dict String (List Event))
feedDecoder feeds =
    D.dict (D.field "entries" (D.list entryDecoder))
        |> D.map (Dict.map (\k -> List.map (eventFromEntry feeds k)))


entryDecoder : D.Decoder Entry
entryDecoder =
    D.map5 Entry
        (D.field "name" D.string)
        (D.field "time" D.int)
        (D.field "link" (D.maybe D.string))
        (D.field "thumbnail" (D.maybe D.string))
        (D.field "description" (D.maybe D.string))


eventFromEntry : Dict String Feeds.Metadata -> String -> Entry -> Event
eventFromEntry feeds key entry =
    Event entry.name
        (Time.millisToPosix (entry.time * 1000))
        entry.link
        entry.thumbnail
        (entry.description
            |> Maybe.map (extractMembers feeds key)
            |> Maybe.withDefault []
        )


extractMembers : Dict String Feeds.Metadata -> String -> String -> List String
extractMembers feeds myKey description =
    feeds
        |> Dict.foldr
            (\key { title } members ->
                if key /= myKey && String.contains ("@" ++ title) description then
                    key :: members

                else
                    members
            )
            []
