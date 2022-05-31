-- Do not manually edit this file, it was auto-generated by yonigibbs/elm-i18next-gen
-- https://github.com/yonigibbs/elm-i18next-gen


module Translations.Error exposing (..)

import I18Next exposing (Delims(..), Translations, customTr, t, tr)


error : Translations -> String
error translations =
    t translations "error.error"


errorCustom : Translations -> (String -> a) -> List a
errorCustom translations nonPlaceholderLift =
    customTr translations Curly nonPlaceholderLift "error.error" []


http : Translations -> String -> String -> String
http translations url msg =
    tr translations Curly "error.http" [ ( "url", url ), ( "msg", msg ) ]


httpCustom : Translations -> (String -> a) -> a -> a -> List a
httpCustom translations nonPlaceholderLift url msg =
    customTr translations Curly nonPlaceholderLift "error.http" [ ( "url", url ), ( "msg", msg ) ]


dismiss : Translations -> String
dismiss translations =
    t translations "error.dismiss"


dismissCustom : Translations -> (String -> a) -> List a
dismissCustom translations nonPlaceholderLift =
    customTr translations Curly nonPlaceholderLift "error.dismiss" []


unexpected : Translations -> String -> String
unexpected translations msg =
    tr translations Curly "error.unexpected" [ ( "msg", msg ) ]


unexpectedCustom : Translations -> (String -> a) -> a -> List a
unexpectedCustom translations nonPlaceholderLift msg =
    customTr translations Curly nonPlaceholderLift "error.unexpected" [ ( "msg", msg ) ]
