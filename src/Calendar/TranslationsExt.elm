-- A dirty hack to handle plurality in i18n.


module Calendar.TranslationsExt exposing (inDuration, startsIn)

import Calendar.Util.Duration as Duration exposing (Duration)
import Html exposing (text, time)
import Html.Attributes exposing (datetime)
import I18Next exposing (Translations)
import Translations.Event as TE


startsIn : Translations -> Duration -> List (Html.Html msg)
startsIn translations duration =
    TE.startsInCustom translations
        text
        (time [ datetime <| Duration.toDatetime duration ]
            [ text <| inDuration translations duration ]
        )


inDuration : Translations -> Duration -> String
inDuration translations duration =
    case Duration.toSmh duration of
        ( _, Just ( _, Just h ) ) ->
            let
                d =
                    h // 24
            in
            if d == 0 then
                trPlural translations TE.anHour TE.nHours h

            else
                trPlural translations TE.aDay TE.nDays d

        ( _, Just ( m, _ ) ) ->
            trPlural translations TE.aMinute TE.nMinutes m

        ( s, _ ) ->
            trPlural translations TE.aSecond TE.nSeconds s


trPlural :
    Translations
    -> (Translations -> String)
    -> (Translations -> String -> String)
    -> Int
    -> String
trPlural translations singular plural n =
    if n == 1 then
        singular translations

    else
        plural translations (String.fromInt n)
