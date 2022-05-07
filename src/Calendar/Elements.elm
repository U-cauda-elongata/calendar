module Calendar.Elements exposing (intlDate, intlTime)

import Calendar.Util.NaiveDate exposing (NaiveDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Time


intlDate : NaiveDate -> Html msg
intlDate date =
    node "intl-date"
        [ attribute "data-year" (String.fromInt date.year)
        , attribute "data-month" (String.fromInt date.month)
        , attribute "data-day" (String.fromInt date.day)
        ]
        []


intlTime : Time.Posix -> Html msg
intlTime time =
    node "intl-time" [ attribute "data-timestamp" (String.fromInt (Time.posixToMillis time)) ] []
