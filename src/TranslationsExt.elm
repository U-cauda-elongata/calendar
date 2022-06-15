-- A dirty hack to handle plurality in i18n.


module TranslationsExt exposing (members, trDuration, viewDuration)

import Duration exposing (Duration)
import Feed
import Html exposing (Html, text, time)
import Html.Attributes exposing (datetime)
import I18Next exposing (Translations)
import Translations.Event as TE
import Translations.Event.Description as TED


members : Translations -> Feed.Preset -> List Feed.Preset -> String
members translations author guests =
    case guests of
        [] ->
            author.title

        [ member ] ->
            TED.twoMembers translations author.title member.title

        _ :: tail ->
            TED.moreThanTwoMembers translations
                author.title
                (String.fromInt <| 1 + List.length tail)


viewDuration : Translations -> Duration -> Html msg
viewDuration translations duration =
    time [ datetime <| Duration.toDatetime duration ]
        [ text <| trDuration translations duration ]


trDuration : Translations -> Duration -> String
trDuration translations duration =
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
