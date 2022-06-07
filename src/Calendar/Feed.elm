module Calendar.Feed exposing (Feed, Preset, decoder)

import Calendar.Event exposing (Event)
import Calendar.Util.Duration as Duration
import Json.Decode as D
import Time


type alias Feed =
    { meta : List Meta
    , entries : List Event
    , next : Maybe String
    }


type alias Meta =
    { id : String
    , title : String
    , alternate : String
    }


type alias Preset =
    { id : String
    , title : String
    , icon : String
    }


decoder : D.Decoder Feed
decoder =
    D.map3 Feed
        (D.oneOf [ D.field "meta" (D.list metaDecoder), D.succeed [] ])
        (D.field "entries" <| D.list eventDecoder)
        (D.maybe <| D.field "next" D.string)


metaDecoder : D.Decoder Meta
metaDecoder =
    D.map3 Meta
        (D.field "id" D.string)
        (D.field "title" D.string)
        (D.field "alternate" D.string)


eventDecoder : D.Decoder Event
eventDecoder =
    D.map Event (D.field "id" D.string)
        |> D.andThen (\f -> D.map f <| D.field "feed" D.string)
        |> D.andThen (\f -> D.map f <| D.field "name" D.string)
        |> D.andThen (\f -> D.map f <| D.oneOf [ D.field "live" D.bool, D.succeed False ])
        |> D.andThen (\f -> D.map f <| D.oneOf [ D.field "upcoming" D.bool, D.succeed False ])
        |> D.andThen
            (\f ->
                D.map f (D.field "time" D.int |> D.map (\time -> Time.millisToPosix <| time * 1000))
            )
        |> D.andThen
            (\f -> D.map f <| D.maybe (D.field "duration" D.int |> D.map Duration.fromMillis))
        |> D.andThen (\f -> D.map f <| D.maybe <| D.field "link" D.string)
        |> D.andThen (\f -> D.map f <| D.maybe <| D.field "thumbnail" D.string)
        |> D.andThen
            (\f -> D.map f <| D.oneOf [ D.field "members" <| D.list D.string, D.succeed [] ])
