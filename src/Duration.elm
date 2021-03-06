module Duration exposing
    ( Duration
    , fromMillis
    , fromSeconds
    , isNegative
    , negate
    , render
    , subPosix
    , toDatetime
    , toSeconds
    , toSmh
    , toSmhd
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Time


type Duration
    = Duration Int


fromMillis : Int -> Duration
fromMillis millis =
    -- Discarding the subsecond part for now. May use at some point in the future.
    fromSeconds <| millis // 1000


fromSeconds : Int -> Duration
fromSeconds =
    Duration


subPosix : Time.Posix -> Time.Posix -> Duration
subPosix t u =
    fromMillis <| Time.posixToMillis t - Time.posixToMillis u


toSeconds : Duration -> Int
toSeconds (Duration secs) =
    secs


toSmh : Duration -> ( Int, Maybe ( Int, Maybe Int ) )
toSmh (Duration secs) =
    case secs // 60 of
        0 ->
            ( secs, Nothing )

        min ->
            let
                secPart =
                    secs |> remainderBy 60
            in
            ( secPart
            , case min // 60 of
                0 ->
                    Just ( min, Nothing )

                h ->
                    Just ( min |> remainderBy 60, Just h )
            )


toSmhd : Duration -> ( Int, Maybe ( Int, Maybe ( Int, Maybe Int ) ) )
toSmhd duration =
    toSmh duration
        |> Tuple.mapSecond
            (Maybe.map
                (Tuple.mapSecond
                    (Maybe.map
                        (\hr ->
                            case hr // 24 of
                                0 ->
                                    ( hr, Nothing )

                                d ->
                                    ( hr |> remainderBy 24, Just d )
                        )
                    )
                )
            )


isNegative : Duration -> Bool
isNegative (Duration secs) =
    secs < 0


negate : Duration -> Duration
negate (Duration secs) =
    fromSeconds -secs


render : Duration -> List (Html msg)
render duration =
    if isNegative duration then
        text "-" :: renderPositive (negate duration)

    else
        renderPositive duration


renderPositive : Duration -> List (Html msg)
renderPositive duration =
    let
        sep =
            span [ class "time-separator" ] [ text ":" ]
    in
    case toSmh duration |> Tuple.mapSecond (Maybe.withDefault ( 0, Nothing )) of
        ( s, ( m, Just h ) ) ->
            [ text <| String.fromInt h
            , sep
            , text <| zeroPad2 <| String.fromInt m
            , sep
            , text <| zeroPad2 <| String.fromInt s
            ]

        ( s, ( m, Nothing ) ) ->
            [ text <| String.fromInt m
            , sep
            , text <| zeroPad2 <| String.fromInt s
            ]


{-| Formats a `Duration` as a valid `datetime` attribute value of `<time>` element.
<https://html.spec.whatwg.org/multipage/common-microsyntaxes.html#valid-duration-string>
-}
toDatetime : Duration -> String
toDatetime duration =
    positiveToDatetime <|
        if isNegative duration then
            negate duration

        else
            duration


positiveToDatetime : Duration -> String
positiveToDatetime duration =
    let
        prepend suffix n text =
            if n == 0 then
                text

            else
                String.fromInt n ++ suffix ++ text

        ( s, mh ) =
            toSmh duration
    in
    "PT"
        ++ (case mh of
                Nothing ->
                    String.fromInt s ++ "S"

                Just ( m, hour ) ->
                    let
                        dt1 =
                            if s == 0 then
                                String.fromInt m ++ "M"

                            else
                                String.fromInt s ++ "S" |> prepend "M" m
                    in
                    case hour of
                        Nothing ->
                            dt1

                        Just h ->
                            dt1 |> prepend "H" h
           )


zeroPad2 : String -> String
zeroPad2 part =
    String.padLeft 2 '0' part
