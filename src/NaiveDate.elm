module NaiveDate exposing (NaiveDate, fromPosix, toIso8601)

import Time
import Util.Time as Time


type alias NaiveDate =
    { year : Int
    , month : Time.Month
    , day : Int
    }


toIso8601 : NaiveDate -> String
toIso8601 d =
    (String.fromInt d.year |> String.padLeft 4 '0')
        ++ "-"
        ++ (String.fromInt (1 + Time.monthToIdx d.month) |> String.padLeft 2 '0')
        ++ "-"
        ++ (String.fromInt d.day |> String.padLeft 2 '0')


fromPosix : Time.Zone -> Time.Posix -> NaiveDate
fromPosix tz t =
    NaiveDate (Time.toYear tz t) (Time.toMonth tz t) (Time.toDay tz t)
