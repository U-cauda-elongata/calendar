module Util.String exposing (stripPrefix, toHalfWidth)


stripPrefix : String -> String -> Maybe String
stripPrefix prefix s =
    if String.startsWith prefix s then
        Just <| String.dropLeft (String.length prefix) s

    else
        Nothing


toHalfWidth : String -> String
toHalfWidth s =
    -- This could be written more neatly like `replace f1 h1 >> replace f2 h2 >> ...`,
    -- but this will result in significantly smaller minified output than its alternatives.
    s
        |> String.replace "\u{3000}" " "
        |> String.replace "！" "!"
        |> String.replace "＂" "\""
        |> String.replace "＃" "#"
        |> String.replace "＄" "$"
        |> String.replace "％" "%"
        |> String.replace "＆" "&"
        |> String.replace "＇" "'"
        |> String.replace "（" "("
        |> String.replace "）" ")"
        |> String.replace "＊" "*"
        |> String.replace "＋" "+"
        |> String.replace "，" ","
        |> String.replace "－" "-"
        |> String.replace "．" "."
        |> String.replace "／" "/"
        |> String.replace "０" "0"
        |> String.replace "１" "1"
        |> String.replace "２" "2"
        |> String.replace "３" "3"
        |> String.replace "４" "4"
        |> String.replace "５" "5"
        |> String.replace "６" "6"
        |> String.replace "７" "7"
        |> String.replace "８" "8"
        |> String.replace "９" "9"
        |> String.replace "：" ":"
        |> String.replace "；" ";"
        |> String.replace "＜" "<"
        |> String.replace "＝" "="
        |> String.replace "＞" ">"
        |> String.replace "？" "?"
        |> String.replace "＠" "@"
        |> String.replace "Ａ" "A"
        |> String.replace "Ｂ" "B"
        |> String.replace "Ｃ" "C"
        |> String.replace "Ｄ" "D"
        |> String.replace "Ｅ" "E"
        |> String.replace "Ｆ" "F"
        |> String.replace "Ｇ" "G"
        |> String.replace "Ｈ" "H"
        |> String.replace "Ｉ" "I"
        |> String.replace "Ｊ" "J"
        |> String.replace "Ｋ" "K"
        |> String.replace "Ｌ" "L"
        |> String.replace "Ｍ" "M"
        |> String.replace "Ｎ" "N"
        |> String.replace "Ｏ" "O"
        |> String.replace "Ｐ" "P"
        |> String.replace "Ｑ" "Q"
        |> String.replace "Ｒ" "R"
        |> String.replace "Ｓ" "S"
        |> String.replace "Ｔ" "T"
        |> String.replace "Ｕ" "U"
        |> String.replace "Ｖ" "V"
        |> String.replace "Ｗ" "W"
        |> String.replace "Ｘ" "X"
        |> String.replace "Ｙ" "Y"
        |> String.replace "Ｚ" "Z"
        |> String.replace "［" "["
        |> String.replace "＼" "\\"
        |> String.replace "］" "]"
        |> String.replace "＾" "^"
        |> String.replace "＿" "_"
        |> String.replace "｀" "`"
        |> String.replace "ａ" "a"
        |> String.replace "ｂ" "b"
        |> String.replace "ｃ" "c"
        |> String.replace "ｄ" "d"
        |> String.replace "ｅ" "e"
        |> String.replace "ｆ" "f"
        |> String.replace "ｇ" "g"
        |> String.replace "ｈ" "h"
        |> String.replace "ｉ" "i"
        |> String.replace "ｊ" "j"
        |> String.replace "ｋ" "k"
        |> String.replace "ｌ" "l"
        |> String.replace "ｍ" "m"
        |> String.replace "ｎ" "n"
        |> String.replace "ｏ" "o"
        |> String.replace "ｐ" "p"
        |> String.replace "ｑ" "q"
        |> String.replace "ｒ" "r"
        |> String.replace "ｓ" "s"
        |> String.replace "ｔ" "t"
        |> String.replace "ｕ" "u"
        |> String.replace "ｖ" "v"
        |> String.replace "ｗ" "w"
        |> String.replace "ｘ" "x"
        |> String.replace "ｙ" "y"
        |> String.replace "ｚ" "z"
        |> String.replace "｛" "{"
        |> String.replace "｜" "|"
        |> String.replace "｝" "}"
        |> String.replace "～" "~"
