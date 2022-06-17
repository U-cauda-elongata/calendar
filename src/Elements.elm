module Elements exposing (dialog, intlDate, intlReltime, intlTime)

import Duration exposing (Duration)
import Html exposing (..)
import Html.Attributes exposing (..)
import NaiveDate exposing (NaiveDate)
import RelativeTime
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


intlReltime : List (Attribute msg) -> Duration -> Html msg
intlReltime attrs duration =
    let
        ( value, unit ) =
            RelativeTime.approximate duration
    in
    node "intl-reltime"
        (attribute "data-value" (String.fromInt value)
            :: attribute "data-unit" (RelativeTime.unitToString unit)
            :: attrs
        )
        []
