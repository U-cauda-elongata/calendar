module TranslationsExt exposing (members)

import I18Next exposing (Translations)
import InteropDefinitions exposing (PresetFeedMeta)
import Translations.Event.Description as TED


members : Translations -> PresetFeedMeta -> List PresetFeedMeta -> String
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
