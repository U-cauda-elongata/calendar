module EventCore exposing (Event, decoder)

{-| An extra module to prevent cyclic import.
-}

import Duration exposing (Duration)
import Json.Decode as D
import Time


type alias Event =
    { id : String
    , feed : String
    , name : String
    , live : Bool
    , upcoming : Bool
    , time : Time.Posix
    , duration : Maybe Duration
    , link : Maybe String
    , thumbnail : Maybe String
    , members : List String
    }


decoder : D.Decoder Event
decoder =
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
