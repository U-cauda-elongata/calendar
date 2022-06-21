module KeyboardEvent exposing (Key, keyDecoder, toTuple)

import Json.Decode as D
import Util.String as String


type alias Key =
    { key : String
    , alt : Bool
    , ctrl : Bool
    , meta : Bool
    , shift : Bool
    }


keyDecoder : D.Decoder Key
keyDecoder =
    D.map5 Key
        codeDecoder
        (D.field "altKey" D.bool)
        (D.field "ctrlKey" D.bool)
        (D.field "metaKey" D.bool)
        (D.field "shiftKey" D.bool)


codeDecoder : D.Decoder String
codeDecoder =
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


{-| Elm's patterns can match against tuple values but cannot match against record fields like
`{ foo = _ }`, so here is a workaround.
-}
toTuple : Key -> ( String, ( Bool, ( Bool, ( Bool, Bool ) ) ) )
toTuple key =
    ( key.key, ( key.alt, ( key.ctrl, ( key.meta, key.shift ) ) ) )
