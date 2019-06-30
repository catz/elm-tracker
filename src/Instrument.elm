module Instrument exposing (Instrument, InstrumentId(..), Sample, decoder, decoderSample, empty, toString)

import Bytes
import Bytes.Decode as Bytes
import Pipeline
import Util


type InstrumentId
    = InstrumentId Int


type alias Instrument =
    { id : InstrumentId
    , name : String
    , length : Int
    , finetune : Int
    , finetuneX : Int
    , volume : Int
    , loop : Loop
    }


type alias Loop =
    { enabled : Bool
    , start : Int
    , length : Int
    , type_ : Int
    }


type alias Sample =
    { id : InstrumentId
    , data : List Float
    }


empty : Instrument
empty =
    Instrument (InstrumentId 0) "empty" 0 0 0 0 (Loop False 0 0 0)


toString : Maybe InstrumentId -> String
toString instrument =
    case instrument of
        Nothing ->
            ".."

        Just (InstrumentId instrumentId) ->
            Util.formatHex instrumentId 2 '0'



-- DECODERS


decoderName : Bytes.Decoder String
decoderName =
    Bytes.string 22
        |> Bytes.andThen
            (\name ->
                name
                    |> String.toList
                    |> List.map
                        (\s ->
                            if Char.toCode s > 0x1F && Char.toCode s < 0x7F then
                                s

                            else
                                ' '
                        )
                    |> String.fromList
                    |> Bytes.succeed
            )


decoderLength : Bytes.Decoder Int
decoderLength =
    Bytes.map ((*) 2)
        (Bytes.unsignedInt16 Bytes.BE)


decoderFinetune : Bytes.Decoder Int
decoderFinetune =
    Bytes.unsignedInt8


decoderVolume : Bytes.Decoder Int
decoderVolume =
    Bytes.unsignedInt8


decoderLoop : Bytes.Decoder Loop
decoderLoop =
    Bytes.map2
        (\start length ->
            let
                enabled =
                    length > 2

                looptype_forward =
                    1
            in
            Loop enabled start length looptype_forward
        )
        (Bytes.map ((*) 2)
            (Bytes.unsignedInt16 Bytes.BE)
        )
        (Bytes.map ((*) 2)
            (Bytes.unsignedInt16 Bytes.BE)
        )


finetuneX : Int -> Int
finetuneX ft =
    if ft > 7 then
        ft - 16

    else
        ft


decoderSample : Instrument -> Bytes.Decoder Sample
decoderSample instrument =
    let
        ignoreFirstTwoBytes =
            List.indexedMap
                (\i x ->
                    if i < 2 then
                        0

                    else
                        x
                )
    in
    Pipeline.decode
        (ignoreFirstTwoBytes >> List.map (\x -> x / 127))
        |> Pipeline.required
            (Pipeline.list instrument.length Bytes.signedInt8
                |> Bytes.map (List.reverse >> List.map toFloat)
            )
        |> Bytes.andThen
            (\data ->
                Bytes.succeed (Sample instrument.id data)
            )


decoder : Int -> Bytes.Decoder Instrument
decoder index =
    let
        fineTuneXInitial =
            0
    in
    Pipeline.decode Instrument
        |> Pipeline.hardcoded (InstrumentId <| index + 1)
        |> Pipeline.required decoderName
        |> Pipeline.required decoderLength
        |> Pipeline.required decoderFinetune
        |> Pipeline.hardcoded fineTuneXInitial
        |> Pipeline.required decoderVolume
        |> Pipeline.required decoderLoop
        |> Bytes.andThen
            (\instrument ->
                Bytes.succeed { instrument | finetuneX = finetuneX instrument.finetune }
            )
