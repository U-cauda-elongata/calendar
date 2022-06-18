module Elements exposing (dialog, intlDate, intlReltime, intlTime)

import Date exposing (Date)
import Duration exposing (Duration)
import Html exposing (..)
import Html.Attributes exposing (..)
import RelativeTime
import Time


dialog : List (Attribute msg) -> List (Html msg) -> Html msg
dialog =
    node "dialog"


intlDate : List (Attribute msg) -> Date -> Html msg
intlDate attrs date =
    node "intl-date"
        (attribute "data-year" (String.fromInt <| Date.year date)
            :: attribute "data-month" (String.fromInt <| Date.monthNumber date - 1)
            :: attribute "data-day" (String.fromInt <| Date.day date)
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
