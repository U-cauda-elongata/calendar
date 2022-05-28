-- A dirty hack to handle plurality in i18n.


module Calendar.TranslationsExt exposing
    ( describeEndedLive
    , describeOngoingLive
    , describeScheduledLive
    , describeVideo
    , startedAgo
    , startsIn
    , trDuration
    )

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


describeOngoingLive :
    Translations
    -> Feed.Preset
    -> List Feed.Preset
    -> List (Html msg)
    -> List (Html msg)
describeOngoingLive translations author guests beganAgo =
    TED.ongoingLiveCustom translations
        (text >> List.singleton)
        (List.singleton <| text <| membersSummary translations author guests)
        beganAgo
        |> List.concat


describeEndedLive :
    Translations
    -> Feed.Preset
    -> List Feed.Preset
    -> Html msg
    -> Html msg
    -> List (Html msg)
describeEndedLive translations author guests time duration =
    TED.endedLiveCustom translations
        text
        (text <| membersSummary translations author guests)
        time
        duration


describeVideo :
    Translations
    -> Feed.Preset
    -> List Feed.Preset
    -> Html msg
    -> Html msg
    -> List (Html msg)
describeVideo translations author guests time duration =
    TED.videoCustom translations
        text
        (text <| membersSummary translations author guests)
        time
        duration


startsIn : Translations -> Duration -> List (Html.Html msg)
startsIn translations duration =
    TE.startsInCustom translations
        text
        (time [ datetime <| Duration.toDatetime duration ]
            [ text <| trDuration translations duration ]
        )


startedAgo : Translations -> Duration -> List (Html msg)
startedAgo translations duration =
    TE.startedAgoCustom translations
        text
        (time [ datetime <| Duration.toDatetime duration ]
            [ text <| trDuration translations duration ]
        )


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
