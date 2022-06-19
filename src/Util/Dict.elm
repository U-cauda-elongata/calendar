module Util.Dict exposing (groupKeysBy, mergeSum, mergeTuple)

import Dict exposing (Dict)


{-| Applies a function to each key of the dictionary and gathers entries with a same output.
-}
groupKeysBy :
    (comparable1 -> comparable2)
    -> Dict comparable1 a
    -> Dict comparable2 (List ( comparable1, a ))
groupKeysBy f dict =
    dict
        |> Dict.foldl
            (\k v acc ->
                acc
                    |> Dict.update (f k)
                        (\list ->
                            case list of
                                Just l ->
                                    Just <| ( k, v ) :: l

                                Nothing ->
                                    Just [ ( k, v ) ]
                        )
            )
            Dict.empty


mergeSum : Dict comparable number -> Dict comparable number -> Dict comparable number
mergeSum xs ys =
    Dict.merge (\k x -> Dict.insert k x)
        (\k x y -> Dict.insert k (x + y))
        (\k y -> Dict.insert k y)
        xs
        ys
        Dict.empty


mergeTuple : Dict comparable a -> Dict comparable b -> Dict comparable ( Maybe a, Maybe b )
mergeTuple xs ys =
    Dict.merge
        (\k x -> Dict.insert k ( Just x, Nothing ))
        (\k x y -> Dict.insert k ( Just x, Just y ))
        (\k y -> Dict.insert k ( Nothing, Just y ))
        xs
        ys
        Dict.empty
