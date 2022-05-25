-- Do not manually edit this file, it was auto-generated by yonigibbs/elm-i18next-gen
-- https://github.com/yonigibbs/elm-i18next-gen


module Translations.Event exposing (..)

import I18Next exposing (Delims(..), Translations, customTr, t, tr)


scheduledFor : Translations -> String -> String -> String
scheduledFor translations time beginsIn =
    tr translations Curly "event.scheduledFor" [ ( "time", time ), ( "beginsIn", beginsIn ) ]


scheduledForCustom : Translations -> (String -> a) -> a -> a -> List a
scheduledForCustom translations nonPlaceholderLift time beginsIn =
    customTr translations Curly nonPlaceholderLift "event.scheduledFor" [ ( "time", time ), ( "beginsIn", beginsIn ) ]


startsIn : Translations -> String -> String
startsIn translations duration =
    tr translations Curly "event.startsIn" [ ( "duration", duration ) ]


startsInCustom : Translations -> (String -> a) -> a -> List a
startsInCustom translations nonPlaceholderLift duration =
    customTr translations Curly nonPlaceholderLift "event.startsIn" [ ( "duration", duration ) ]


aDay : Translations -> String
aDay translations =
    t translations "event.aDay"


aDayCustom : Translations -> (String -> a) -> List a
aDayCustom translations nonPlaceholderLift =
    customTr translations Curly nonPlaceholderLift "event.aDay" []


nDays : Translations -> String -> String
nDays translations n =
    tr translations Curly "event.nDays" [ ( "n", n ) ]


nDaysCustom : Translations -> (String -> a) -> a -> List a
nDaysCustom translations nonPlaceholderLift n =
    customTr translations Curly nonPlaceholderLift "event.nDays" [ ( "n", n ) ]


anHour : Translations -> String
anHour translations =
    t translations "event.anHour"


anHourCustom : Translations -> (String -> a) -> List a
anHourCustom translations nonPlaceholderLift =
    customTr translations Curly nonPlaceholderLift "event.anHour" []


nHours : Translations -> String -> String
nHours translations n =
    tr translations Curly "event.nHours" [ ( "n", n ) ]


nHoursCustom : Translations -> (String -> a) -> a -> List a
nHoursCustom translations nonPlaceholderLift n =
    customTr translations Curly nonPlaceholderLift "event.nHours" [ ( "n", n ) ]


aMinute : Translations -> String
aMinute translations =
    t translations "event.aMinute"


aMinuteCustom : Translations -> (String -> a) -> List a
aMinuteCustom translations nonPlaceholderLift =
    customTr translations Curly nonPlaceholderLift "event.aMinute" []


nMinutes : Translations -> String -> String
nMinutes translations n =
    tr translations Curly "event.nMinutes" [ ( "n", n ) ]


nMinutesCustom : Translations -> (String -> a) -> a -> List a
nMinutesCustom translations nonPlaceholderLift n =
    customTr translations Curly nonPlaceholderLift "event.nMinutes" [ ( "n", n ) ]


aSecond : Translations -> String
aSecond translations =
    t translations "event.aSecond"


aSecondCustom : Translations -> (String -> a) -> List a
aSecondCustom translations nonPlaceholderLift =
    customTr translations Curly nonPlaceholderLift "event.aSecond" []


nSeconds : Translations -> String -> String
nSeconds translations n =
    tr translations Curly "event.nSeconds" [ ( "n", n ) ]


nSecondsCustom : Translations -> (String -> a) -> a -> List a
nSecondsCustom translations nonPlaceholderLift n =
    customTr translations Curly nonPlaceholderLift "event.nSeconds" [ ( "n", n ) ]


startedAt : Translations -> String -> String
startedAt translations time =
    tr translations Curly "event.startedAt" [ ( "time", time ) ]


startedAtCustom : Translations -> (String -> a) -> a -> List a
startedAtCustom translations nonPlaceholderLift time =
    customTr translations Curly nonPlaceholderLift "event.startedAt" [ ( "time", time ) ]


uploadedAt : Translations -> String -> String
uploadedAt translations time =
    tr translations Curly "event.uploadedAt" [ ( "time", time ) ]


uploadedAtCustom : Translations -> (String -> a) -> a -> List a
uploadedAtCustom translations nonPlaceholderLift time =
    customTr translations Curly nonPlaceholderLift "event.uploadedAt" [ ( "time", time ) ]
