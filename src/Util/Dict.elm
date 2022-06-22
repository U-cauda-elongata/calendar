module Util.Dict exposing (groupKeysBy, mergeSum, mergeTuple)

import Dict exposing (Dict)


{-| Applies a function to each key of the dictionary and gathers entries with a same output.
-}
groupKeysBy :
    (comparable1 -> comparable2)
    -> Dict comparable1 a
    -> Dict comparable2 (List ( comparable1, a ))
groupKeysBy f =
    Dict.foldl
        (\k v -> Dict.update (f k) (Just << (::) ( k, v ) << Maybe.withDefault []))
        Dict.empty


mergeSum : Dict comparable number -> Dict comparable number -> Dict comparable number
mergeSum =
    Dict.foldl (\k x -> Dict.update k (Just << (+) x << Maybe.withDefault 0))


mergeTuple : Dict comparable a -> Dict comparable b -> Dict comparable ( Maybe a, Maybe b )
mergeTuple xs ys =
    Dict.merge
        (\k x -> Dict.insert k ( Just x, Nothing ))
        (\k x y -> Dict.insert k ( Just x, Just y ))
        (\k y -> Dict.insert k ( Nothing, Just y ))
        xs
        ys
        Dict.empty
