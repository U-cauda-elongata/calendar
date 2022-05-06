module Calendar.Util exposing (NaiveDate, cardinalities, groupBy, toNaiveDate)

import Dict exposing (Dict)
import Time



-- LIST


cardinalities : List comparable -> Dict comparable Int
cardinalities list =
    list
        |> List.foldl (\v -> Dict.update v (\n -> Just (1 + Maybe.withDefault 0 n)))
            Dict.empty


groupBy : (a -> b) -> List a -> List ( b, List a )
groupBy pred list =
    list
        |> List.foldr
            (\a ->
                \groups ->
                    let
                        b =
                            pred a
                    in
                    case groups of
                        ( bhead, as_ ) :: tail ->
                            if bhead == b then
                                ( bhead, a :: as_ ) :: tail

                            else
                                ( b, [ a ] ) :: groups

                        _ ->
                            [ ( b, [ a ] ) ]
            )
            []



-- NAIVEDATE


type alias NaiveDate =
    { year : Int
    , month : Int
    , day : Int
    }


toNaiveDate : Time.Zone -> Time.Posix -> NaiveDate
toNaiveDate tz t =
    NaiveDate (Time.toYear tz t) (monthToInt (Time.toMonth tz t)) (Time.toDay tz t)


monthToInt : Time.Month -> Int
monthToInt month =
    case month of
        Time.Jan ->
            0

        Time.Feb ->
            1

        Time.Mar ->
            2

        Time.Apr ->
            3

        Time.May ->
            4

        Time.Jun ->
            5

        Time.Jul ->
            6

        Time.Aug ->
            7

        Time.Sep ->
            8

        Time.Oct ->
            9

        Time.Nov ->
            10

        Time.Dec ->
            11
