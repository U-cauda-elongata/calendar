module Calendar.Util exposing (cardinalities, groupBy, stripPrefix)

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



-- STRING


stripPrefix : String -> String -> Maybe String
stripPrefix prefix s =
    if String.startsWith prefix s then
        Just <| String.dropLeft (String.length prefix) s

    else
        Nothing
