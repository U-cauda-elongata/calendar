module Calendar.Util exposing (cardinalities, groupBy, mergeBy, stripPrefix)

import Dict exposing (Dict)



-- LIST


cardinalities : List comparable -> Dict comparable Int
cardinalities list =
    list
        |> List.foldl (\v -> Dict.update v (\n -> Just <| 1 + Maybe.withDefault 0 n))
            Dict.empty


groupBy : (a -> b) -> List a -> List ( b, List a )
groupBy pred list =
    list
        |> List.foldr
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
mergeBy f old new =
    -- XXX: This could be done more efficiently if there'd be an implementation of `Dict`
    -- that preserves ordering.
    new
        |> List.foldl
            (\n acc1 ->
                let
                    fn =
                        f n

                    ( acc, replacing ) =
                        acc1
                            |> List.foldl
                                (\o ( acc2, r ) ->
                                    if r && fn == f o then
                                        ( n :: acc2, False )

                                    else
                                        ( o :: acc2, r )
                                )
                                ( [], True )
                in
                if replacing then
                    n :: acc

                else
                    acc
            )
            old



-- STRING


stripPrefix : String -> String -> Maybe String
stripPrefix prefix s =
    if String.startsWith prefix s then
        Just <| String.dropLeft (String.length prefix) s

    else
        Nothing
