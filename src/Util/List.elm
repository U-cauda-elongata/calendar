module Util.List exposing (cardinalities, groupBy, mergeBy)

import Dict exposing (Dict)


cardinalities : List comparable -> Dict comparable Int
cardinalities =
    List.foldl (\v -> Dict.update v (\n -> Just <| 1 + Maybe.withDefault 0 n)) Dict.empty


groupBy : (a -> b) -> List a -> List ( b, List a )
groupBy pred =
    List.foldr
        (\a groups ->
            let
                b =
                    pred a
            in
            case groups of
                ( bhead, as_ ) :: tail ->
                    if bhead == b then
                        ( bhead, a :: as_ ) :: tail

                    else
                        ( b, [ a ] ) :: groups

                _ ->
                    [ ( b, [ a ] ) ]
        )
        []


{-| Merge the second list into the first list, i.e., for each element in the second list, replace
with it an equal element in the first list if any, or insert it into the first list otherwise.
-}
mergeBy : (a -> b) -> List a -> List a -> List a
mergeBy f =
    -- XXX: This could be done more efficiently if there'd be an implementation of `Dict`
    -- that preserves ordering.
    List.foldl
        (\new acc1 ->
            let
                fn =
                    f new

                ( acc, replacing ) =
                    acc1
                        |> List.foldl
                            (\old ( acc2, r ) ->
                                if r && fn == f old then
                                    ( new :: acc2, False )

                                else
                                    ( old :: acc2, r )
                            )
                            ( [], True )
            in
            if replacing then
                new :: acc

            else
                acc
        )
