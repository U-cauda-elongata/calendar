module Calendar.Util.Duration exposing (Duration, fromMillis, fromSeconds, negate, render, toDatetime, toSeconds, toSmh)

import Html exposing (..)
import Html.Attributes exposing (..)


type Duration
    = Duration Int


fromMillis : Int -> Duration
fromMillis millis =
    -- Discarding the subsecond part for now. May use at some point in the future.
    fromSeconds (millis // 1000)


fromSeconds : Int -> Duration
fromSeconds secs =
    Duration secs


toSeconds : Duration -> Int
toSeconds duration =
    case duration of
        Duration secs ->
            secs


toSmh : Duration -> ( Int, Maybe ( Int, Maybe Int ) )
toSmh duration =
    let
        sec =
            toSeconds duration
    in
    case sec // 60 of
        0 ->
            ( sec, Nothing )

        min ->
            let
                secPart =
                    sec |> modBy 60
            in
            case min // 60 of
                0 ->
                    ( secPart, Just ( min, Nothing ) )

                h ->
                    ( secPart, Just ( min |> modBy 60, Just h ) )


negate : Duration -> Duration
negate duration =
    case duration of
        Duration secs ->
            fromSeconds -secs


render : Duration -> List (Html msg)
render duration =
    let
        sep =
            span [ class "time-separator" ] [ text ":" ]
    in
    case toSmh duration |> Tuple.mapSecond (Maybe.withDefault ( 0, Nothing )) of
        ( s, ( m, Just h ) ) ->
            [ text <| String.fromInt h
            , sep
            , text (String.fromInt m |> zeroPad2)
            , sep
            , text (String.fromInt s |> zeroPad2)
            ]

        ( s, ( m, Nothing ) ) ->
            [ text <| String.fromInt m
            , sep
            , text (String.fromInt s |> zeroPad2)
            ]


{-| Formats a `Duration` as a valid `datetime` attribute value of `<time>` element.
<https://html.spec.whatwg.org/multipage/common-microsyntaxes.html#valid-duration-string>
-}
toDatetime : Duration -> String
toDatetime duration =
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
                                prepend "M" m (String.fromInt s ++ "S")
                    in
                    case hour of
                        Nothing ->
                            dt1

                        Just h ->
                            prepend "H" h dt1
           )


zeroPad2 : String -> String
zeroPad2 part =
    String.padLeft 2 '0' part
