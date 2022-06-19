module Util.String exposing (stripPrefix)


stripPrefix : String -> String -> Maybe String
stripPrefix prefix s =
    if String.startsWith prefix s then
        Just <| String.dropLeft (String.length prefix) s

    else
        Nothing
