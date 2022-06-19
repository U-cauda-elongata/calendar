module KeyboardEvent exposing (keyDecoder)

import Json.Decode as D
import Util.String as String


keyDecoder : D.Decoder String
keyDecoder =
    D.field "code" D.string
        |> D.andThen
            (\code ->
                -- Special-casing for digit keys because we want to match on combinations like
                -- `<S-1>`, which maps to `key` value of `!` on US QWERTY layout for example.
                case code |> String.stripPrefix "Digit" of
                    Just n ->
                        D.succeed n

                    Nothing ->
                        D.field "key" D.string |> D.map String.toUpper
            )
        |> D.andThen (appendModifier "shiftKey" "S-")
        |> D.andThen (appendModifier "metaKey" "M-")
        |> D.andThen (appendModifier "ctrlKey" "C-")
        |> D.andThen (appendModifier "altKey" "A-")


appendModifier : String -> String -> String -> D.Decoder String
appendModifier field prefix key =
    D.oneOf [ D.field field D.bool, D.succeed False ]
        |> D.map
            (\value ->
                if value then
                    prefix ++ key

                else
                    key
            )
