module Util exposing (formatHex, formatHexExtended)

import ParseInt


formatHex : Int -> Int -> Char -> String
formatHex i len pad =
    ParseInt.toHex i |> String.padLeft len pad |> String.toUpper


formatHexExtended : Int -> Int -> Char -> String
formatHexExtended i len pad =
    ParseInt.toRadix 32 i |> Result.withDefault "" |> String.padLeft len pad |> String.toUpper
