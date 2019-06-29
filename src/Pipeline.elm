module Pipeline exposing (custom, decode, hardcoded, list, listCustom, required)

import Bytes.Decode as Decode exposing (Decoder, Step(..), loop, map)


custom : Decoder a -> Decoder (a -> b) -> Decoder b
custom decoder wrapped =
    Decode.map2 (<|) wrapped decoder


decode : a -> Decoder a
decode =
    Decode.succeed


required : Decoder a -> Decoder (a -> b) -> Decoder b
required valDecoder decoder =
    custom valDecoder decoder


list : Int -> Decoder a -> Decoder (List a)
list len decoder =
    loop ( len, [] ) (listStep decoder)


listStep : Decoder a -> ( Int, List a ) -> Decoder (Step ( Int, List a ) (List a))
listStep decoder ( n, xs ) =
    if n <= 0 then
        Decode.succeed (Done xs)

    else
        map (\x -> Loop ( n - 1, x :: xs )) decoder


listCustom : List a -> (a -> Decoder b) -> Decoder (List b)
listCustom lens decoder =
    loop ( lens, [] ) (listStepCustom decoder)


listStepCustom : (a -> Decoder b) -> ( List a, List b ) -> Decoder (Step ( List a, List b ) (List b))
listStepCustom decoder ( ls, xs ) =
    case ls of
        [] ->
            Decode.succeed (Done xs)

        l :: ls2 ->
            map (\x -> Loop ( ls2, x :: xs )) (decoder l)


hardcoded : a -> Decoder (a -> b) -> Decoder b
hardcoded =
    Decode.succeed >> custom
