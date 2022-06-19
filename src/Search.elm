module Search exposing (matches, suggestions)

import Dict
import List.Extra as List
import Regex exposing (Regex)
import Time exposing (Posix)
import Util.Dict as Dict
import Util.List as List
import Util.String as String


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


{-| Takes a list of items each having a text and associated time, and returns a list of "tags" in
the texts ordered by how they are "trending" at the specified period of time.

"Trendiness" of a tag is determined by the number of occurence in the past 7 days divided by that
of the previous 7 days. If the tag has not appeared in the past 14 days but has appeared before
that, it has trendingness of zero.

-}
suggestions : (a -> String) -> (a -> Posix) -> Posix -> List a -> List String
suggestions getText getTime now xs =
    let
        getTimeMs =
            Time.posixToMillis << getTime

        week =
            7 * 24 * 60 * 60 * 1000

        weekBefore =
            Time.posixToMillis now - week

        ( current, recent, past ) =
            let
                ( c, tail1 ) =
                    xs
                        |> List.splitWhen (\x -> getTimeMs x < weekBefore)
                        |> Maybe.withDefault ( xs, [] )

                ( r, p ) =
                    tail1
                        |> List.splitWhen (\x -> getTimeMs x < weekBefore - week)
                        |> Maybe.withDefault ( xs, [] )
            in
            ( c, r, p )

        countTags list =
            list
                |> List.concatMap (searchTags << getText)
                |> List.cardinalities
                |> Dict.groupKeysBy normalize

        mergedRecent =
            Dict.mergeTuple (countTags current) (countTags recent)

        head =
            mergedRecent
                |> Dict.values
                |> List.filterMap
                    (\( currentPairs, recentPairs ) ->
                        let
                            cp =
                                currentPairs |> Maybe.withDefault []

                            rp =
                                recentPairs |> Maybe.withDefault []
                        in
                        -- Use the most commonly-used form among unnormalized tags (rather than the
                        -- "most trendy form", which does not even make sense).
                        Dict.mergeSum (Dict.fromList cp) (Dict.fromList rp)
                            |> Dict.toList
                            |> List.maximumBy Tuple.second
                            -- This should never produce `Nothing` though.
                            |> Maybe.map
                                (\( tag, _ ) ->
                                    let
                                        sum =
                                            List.map Tuple.second >> List.sum >> toFloat

                                        scp =
                                            sum cp

                                        srp =
                                            sum rp

                                        trendiness =
                                            -- Add ones to avoid `Infinity`.
                                            (scp + 1) / (srp + 1)
                                    in
                                    -- Use the total number of occurences for ordering
                                    -- when the trendinesses are equal.
                                    ( tag, ( -trendiness, -(scp + srp) ) )
                                )
                    )

        tail =
            Dict.diff (countTags past) mergedRecent
                |> Dict.values
                |> List.filterMap
                    (\pairs ->
                        pairs
                            |> List.maximumBy Tuple.second
                            |> Maybe.map
                                (\( tag, _ ) ->
                                    let
                                        sum =
                                            pairs |> List.map Tuple.second |> List.sum |> toFloat
                                    in
                                    ( tag, ( 0, -sum ) )
                                )
                    )
    in
    (head ++ tail) |> List.sortBy Tuple.second |> List.map Tuple.first


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
    text |> String.toUpper |> String.toHalfWidth
