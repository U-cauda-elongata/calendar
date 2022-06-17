module TranslationsExt exposing (members)

import Feed
import I18Next exposing (Translations)
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
