module Elements exposing (dialog, intlDate, intlTime)

import Html exposing (..)
import Html.Attributes exposing (..)
import NaiveDate exposing (NaiveDate)
import Time
import Util.Time as Time


dialog : List (Attribute msg) -> List (Html msg) -> Html msg
dialog =
    node "dialog"


intlDate : List (Attribute msg) -> NaiveDate -> Html msg
intlDate attrs date =
    node "intl-date"
        (attribute "data-year" (String.fromInt date.year)
            :: attribute "data-month" (String.fromInt <| Time.monthToIdx date.month)
            :: attribute "data-day" (String.fromInt date.day)
            :: attrs
        )
        []


intlTime : List (Attribute msg) -> Time.Posix -> Html msg
intlTime attrs time =
    node "intl-time"
        (attribute "data-timestamp" (String.fromInt <| Time.posixToMillis time) :: attrs)
        []