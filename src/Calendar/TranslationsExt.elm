-- A dirty hack to handle plurality in i18n.


module Calendar.TranslationsExt exposing (describeScheduledLive, describeStartedLive, describeVideo, inDuration, startsIn)

import Calendar.Feed as Feed
import Calendar.Util.Duration as Duration exposing (Duration)
import Html exposing (Html, text, time)
import Html.Attributes exposing (datetime)
import I18Next exposing (Translations)
import Translations.Event as TE
import Translations.Event.Description as TED


describeScheduledLive :
    Translations
    -> Feed.Preset
    -> List Feed.Preset
    -> List (Html msg)
    -> List (Html msg)
describeScheduledLive translations author guests beginsIn =
    TED.scheduledLiveCustom translations
        (text >> List.singleton)
        (List.singleton <| text <| membersSummary translations author guests)
        beginsIn
        |> List.concat


describeStartedLive :
    Translations
    -> Feed.Preset
    -> List Feed.Preset
    -> Html msg
    -> List (Html msg)
describeStartedLive translations author guests time =
    TED.startedLiveCustom translations
        text
        (text <| membersSummary translations author guests)
        time


describeVideo :
    Translations
    -> Feed.Preset
    -> List Feed.Preset
    -> Html msg
    -> List (Html msg)
describeVideo translations author guests time =
    TED.startedLiveCustom translations
        text
        (text <| membersSummary translations author guests)
        time


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


membersSummary : Translations -> Feed.Preset -> List Feed.Preset -> String
membersSummary translations author guests =
    case guests of
        [] ->
            author.title

        [ member ] ->
            TED.twoMembers translations author.title member.title

        _ :: tail ->
            TED.moreThanTwoMembers translations
                author.title
                (String.fromInt <| 1 + List.length tail)
