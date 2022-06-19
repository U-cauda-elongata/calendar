module Search exposing (matches, suggestions)

import Dict
import List.Extra as List
import Regex exposing (Regex)
import Util.Dict as Dict
import Util.List as List


matches : String -> String -> Bool
matches needle haystack =
    let
        hs =
            normalize haystack
    in
    String.isEmpty needle
        || (normalize needle
                |> String.words
                |> List.all (\term -> hs |> String.contains term)
           )


suggestions : (a -> String) -> List a -> List String
suggestions getText xs =
    xs
        |> List.concatMap (searchTags << getText)
        |> List.cardinalities
        |> Dict.groupKeysBy normalize
        |> Dict.values
        |> List.filterMap
            (\pairs ->
                pairs
                    -- Use the most common form among unnormalized terms.
                    |> List.maximumBy Tuple.second
                    -- This should never produce `Nothing` though.
                    |> Maybe.map
                        (\( tag, _ ) -> ( tag, pairs |> List.map Tuple.second |> List.sum ))
            )
        |> List.sortBy (Tuple.second >> negate)
        |> List.map Tuple.first


tagRe : Regex
tagRe =
    Regex.fromString "【([^】]*)】" |> Maybe.withDefault Regex.never


slashesRe : Regex
slashesRe =
    Regex.fromString "[/／]" |> Maybe.withDefault Regex.never


searchTags : String -> List String
searchTags string =
    Regex.find tagRe string
        |> List.filterMap (\match -> List.head match.submatches |> Maybe.andThen identity)
        |> List.concatMap (Regex.split slashesRe)
        |> List.map String.trim


normalize : String -> String
normalize text =
    text |> String.toUpper |> String.replace "＃" "#"
