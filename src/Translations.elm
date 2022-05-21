-- Do not manually edit this file, it was auto-generated by yonigibbs/elm-i18next-gen
-- https://github.com/yonigibbs/elm-i18next-gen


module Translations exposing (..)

import I18Next exposing (Delims(..), Translations, customTr, t, tr)


title : Translations -> String
title translations =
    t translations "title"


titleCustom : Translations -> (String -> a) -> List a
titleCustom translations nonPlaceholderLift =
    customTr translations Curly nonPlaceholderLift "title" []


filterMenuLabel : Translations -> String
filterMenuLabel translations =
    t translations "filterMenuLabel"


filterMenuLabelCustom : Translations -> (String -> a) -> List a
filterMenuLabelCustom translations nonPlaceholderLift =
    customTr translations Curly nonPlaceholderLift "filterMenuLabel" []


clearFilter : Translations -> String
clearFilter translations =
    t translations "clearFilter"


clearFilterCustom : Translations -> (String -> a) -> List a
clearFilterCustom translations nonPlaceholderLift =
    customTr translations Curly nonPlaceholderLift "clearFilter" []


search : Translations -> String
search translations =
    t translations "search"


searchCustom : Translations -> (String -> a) -> List a
searchCustom translations nonPlaceholderLift =
    customTr translations Curly nonPlaceholderLift "search" []


feedFilterLabel : Translations -> String
feedFilterLabel translations =
    t translations "feedFilterLabel"


feedFilterLabelCustom : Translations -> (String -> a) -> List a
feedFilterLabelCustom translations nonPlaceholderLift =
    customTr translations Curly nonPlaceholderLift "feedFilterLabel" []


avatarAlt : Translations -> String
avatarAlt translations =
    t translations "avatarAlt"


avatarAltCustom : Translations -> (String -> a) -> List a
avatarAltCustom translations nonPlaceholderLift =
    customTr translations Curly nonPlaceholderLift "avatarAlt" []


nowSeparator : Translations -> String -> String
nowSeparator translations time =
    tr translations Curly "nowSeparator" [ ( "time", time ) ]


nowSeparatorCustom : Translations -> (String -> a) -> a -> List a
nowSeparatorCustom translations nonPlaceholderLift time =
    customTr translations Curly nonPlaceholderLift "nowSeparator" [ ( "time", time ) ]


thumbnailAlt : Translations -> String
thumbnailAlt translations =
    t translations "thumbnailAlt"


thumbnailAltCustom : Translations -> (String -> a) -> List a
thumbnailAltCustom translations nonPlaceholderLift =
    customTr translations Curly nonPlaceholderLift "thumbnailAlt" []


emptyResultPre : Translations -> String
emptyResultPre translations =
    t translations "emptyResultPre"


emptyResultPreCustom : Translations -> (String -> a) -> List a
emptyResultPreCustom translations nonPlaceholderLift =
    customTr translations Curly nonPlaceholderLift "emptyResultPre" []


emptyResultKidding : Translations -> String
emptyResultKidding translations =
    t translations "emptyResultKidding"


emptyResultKiddingCustom : Translations -> (String -> a) -> List a
emptyResultKiddingCustom translations nonPlaceholderLift =
    customTr translations Curly nonPlaceholderLift "emptyResultKidding" []


emptyResultPost : Translations -> String
emptyResultPost translations =
    t translations "emptyResultPost"


emptyResultPostCustom : Translations -> (String -> a) -> List a
emptyResultPostCustom translations nonPlaceholderLift =
    customTr translations Curly nonPlaceholderLift "emptyResultPost" []
