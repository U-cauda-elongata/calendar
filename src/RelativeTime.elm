module RelativeTime exposing (Unit, approximate, unitToString)

import Duration exposing (Duration)


type Unit
    = Day
    | Hour
    | Minute
    | Second


approximate : Duration -> ( Int, Unit )
approximate duration =
    case Duration.toSmhd duration of
        ( s, Nothing ) ->
            ( s, Second )

        ( _, Just ( m, Nothing ) ) ->
            ( m, Minute )

        ( _, Just ( _, Just ( h, Nothing ) ) ) ->
            ( h, Hour )

        ( _, Just ( _, Just ( _, Just d ) ) ) ->
            ( d, Day )


unitToString : Unit -> String
unitToString unit =
    case unit of
        Day ->
            "day"

        Hour ->
            "hour"

        Minute ->
            "minute"

        Second ->
            "second"
