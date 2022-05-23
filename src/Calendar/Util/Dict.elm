module Calendar.Util.Dict exposing (groupKeysBy)

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
