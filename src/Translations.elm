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


collapseMenu : Translations -> String
collapseMenu translations =
    t translations "collapseMenu"


collapseMenuCustom : Translations -> (String -> a) -> List a
collapseMenuCustom translations nonPlaceholderLift =
    customTr translations Curly nonPlaceholderLift "collapseMenu" []


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


ongoing : Translations -> String -> String
ongoing translations time =
    tr translations Curly "ongoing" [ ( "time", time ) ]


ongoingCustom : Translations -> (String -> a) -> a -> List a
ongoingCustom translations nonPlaceholderLift time =
    customTr translations Curly nonPlaceholderLift "ongoing" [ ( "time", time ) ]


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


loadMore : Translations -> String
loadMore translations =
    t translations "loadMore"


loadMoreCustom : Translations -> (String -> a) -> List a
loadMoreCustom translations nonPlaceholderLift =
    customTr translations Curly nonPlaceholderLift "loadMore" []


loadMoreLabel : Translations -> String
loadMoreLabel translations =
    t translations "loadMoreLabel"


loadMoreLabelCustom : Translations -> (String -> a) -> List a
loadMoreLabelCustom translations nonPlaceholderLift =
    customTr translations Curly nonPlaceholderLift "loadMoreLabel" []


retryLoading : Translations -> String
retryLoading translations =
    t translations "retryLoading"


retryLoadingCustom : Translations -> (String -> a) -> List a
retryLoadingCustom translations nonPlaceholderLift =
    customTr translations Curly nonPlaceholderLift "retryLoading" []


retryLoadingLabel : Translations -> String
retryLoadingLabel translations =
    t translations "retryLoadingLabel"


retryLoadingLabelCustom : Translations -> (String -> a) -> List a
retryLoadingLabelCustom translations nonPlaceholderLift =
    customTr translations Curly nonPlaceholderLift "retryLoadingLabel" []


loading : Translations -> String
loading translations =
    t translations "loading"


loadingCustom : Translations -> (String -> a) -> List a
loadingCustom translations nonPlaceholderLift =
    customTr translations Curly nonPlaceholderLift "loading" []


noMoreItems : Translations -> String
noMoreItems translations =
    t translations "noMoreItems"


noMoreItemsCustom : Translations -> (String -> a) -> List a
noMoreItemsCustom translations nonPlaceholderLift =
    customTr translations Curly nonPlaceholderLift "noMoreItems" []


noMoreItemsGibberish : Translations -> String
noMoreItemsGibberish translations =
    t translations "noMoreItemsGibberish"


noMoreItemsGibberishCustom : Translations -> (String -> a) -> List a
noMoreItemsGibberishCustom translations nonPlaceholderLift =
    customTr translations Curly nonPlaceholderLift "noMoreItemsGibberish" []


closeDialog : Translations -> String
closeDialog translations =
    t translations "closeDialog"


closeDialogCustom : Translations -> (String -> a) -> List a
closeDialogCustom translations nonPlaceholderLift =
    customTr translations Curly nonPlaceholderLift "closeDialog" []


goBackTo : Translations -> String -> String
goBackTo translations section =
    tr translations Curly "goBackTo" [ ( "section", section ) ]


goBackToCustom : Translations -> (String -> a) -> a -> List a
goBackToCustom translations nonPlaceholderLift section =
    customTr translations Curly nonPlaceholderLift "goBackTo" [ ( "section", section ) ]
