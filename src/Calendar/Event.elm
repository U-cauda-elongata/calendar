module Calendar.Event exposing (Event, feedDecoder)

import Calendar.Feeds as Feeds
import Calendar.Util.Duration as Duration exposing (Duration)
import Json.Decode as D
import List.Extra
import Time


type alias Event =
    { name : String
    , live : Bool
    , time : Time.Posix
    , duration : Maybe Duration
    , link : Maybe String
    , thumbnail : Maybe String
    , members : List Int
    }


type alias Entry =
    { name : String
    , live : Bool
    , time : Int
    , duration : Maybe Int
    , link : Maybe String
    , thumbnail : Maybe String
    , description : Maybe String
    }


feedDecoder : List Feeds.Metadata -> D.Decoder (List Event)
feedDecoder feeds =
    D.field "entries" (D.list entryDecoder)
        |> D.map (\entries -> entries |> List.map (eventFromEntry feeds))


entryDecoder : D.Decoder Entry
entryDecoder =
    D.map7 Entry
        (D.field "name" D.string)
        (D.oneOf [ D.field "live" D.bool, D.succeed False ])
        (D.field "time" D.int)
        (D.maybe (D.field "duration" D.int))
        (D.maybe (D.field "link" D.string))
        (D.maybe (D.field "thumbnail" D.string))
        (D.maybe (D.field "description" D.string))


eventFromEntry : List Feeds.Metadata -> Entry -> Event
eventFromEntry feeds entry =
    Event entry.name
        entry.live
        (Time.millisToPosix (entry.time * 1000))
        (entry.duration |> Maybe.map Duration.fromMillis)
        entry.link
        entry.thumbnail
        (entry.description
            |> Maybe.map (extractMembers feeds)
            |> Maybe.withDefault []
        )


extractMembers : List Feeds.Metadata -> String -> List Int
extractMembers feeds description =
    feeds
        |> List.Extra.indexedFoldr
            (\i { title } members ->
                if description |> String.contains ("@" ++ title) then
                    i :: members

                else
                    members
            )
            []
