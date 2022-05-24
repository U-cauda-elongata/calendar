module Calendar.Util.Duration exposing (Duration, format, fromSeconds, toDatetime, toSeconds, toSmh)


type Duration
    = Duration Int


fromSeconds : Int -> Duration
fromSeconds secs =
    Duration secs


toSeconds : Duration -> Int
toSeconds duration =
    case duration of
        Duration secs ->
            secs


toSmhd : Duration -> ( Int, Maybe ( Int, Maybe ( Int, Maybe Int ) ) )
toSmhd duration =
    case toSmh duration of
        ( s, Just ( m, Just h ) ) ->
            case h // 24 of
                0 ->
                    ( s, Just ( m, Just ( h, Nothing ) ) )

                d ->
                    ( s, Just ( m, Just ( h |> modBy 24, Just d ) ) )

        ( s, mh ) ->
            -- The `h` part is always a `Nothing : Maybe Int` here,
            -- but we have to map it to a `Nothing : Maybe (Int, Maybe Int)`.
            ( s, mh |> Maybe.map (Tuple.mapSecond (always Nothing)) )


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


format : Duration -> String
format duration =
    case toSmh duration |> Tuple.mapSecond (Maybe.withDefault ( 0, Nothing )) of
        ( s, ( m, Just h ) ) ->
            String.fromInt h
                ++ ":"
                ++ (String.fromInt m |> zeroPad2)
                ++ ":"
                ++ (String.fromInt s |> zeroPad2)

        ( s, ( m, Nothing ) ) ->
            String.fromInt m ++ ":" ++ String.fromInt s |> zeroPad2


{-| Formats a `Duration` as a valid `datetime` attribute value of `<time>` element.
<https://html.spec.whatwg.org/multipage/common-microsyntaxes.html#valid-duration-string>
-}
toDatetime : Duration -> String
toDatetime duration =
    let
        ( s, mhd ) =
            toSmhd duration

        dt =
            String.fromInt s
    in
    case mhd of
        Nothing ->
            "PT" ++ dt

        Just ( m, hd ) ->
            let
                dt2 =
                    String.fromInt m ++ "M" ++ dt
            in
            case hd of
                Nothing ->
                    "PT" ++ dt2

                Just ( h, day ) ->
                    let
                        dt3 =
                            String.fromInt h ++ "H" ++ dt2
                    in
                    case day of
                        Nothing ->
                            "PT" ++ dt3

                        Just d ->
                            "P" ++ String.fromInt d ++ "DT" ++ dt3


zeroPad2 : String -> String
zeroPad2 part =
    String.padLeft 2 '0' part
