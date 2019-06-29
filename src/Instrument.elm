module Instrument exposing (Instrument, InstrumentId(..), decoder, decoderSample, toString)

import Bytes
import Bytes.Decode as Bytes
import Pipeline
import Util


type InstrumentId
    = InstrumentId Int


type alias Instrument =
    { name : String
    , type_ : String
    , sample : Sample
    , id : InstrumentId
    }


type alias Sample =
    { name : String
    , length : Int
    , finetune : Int
    , finetuneX : Int
    , volume : Int
    , loop : Loop
    , data : List Float
    }


type alias Loop =
    { enabled : Bool
    , start : Int
    , length : Int
    , type_ : Int
    }


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


decodeSample : Bytes.Decoder Sample
decodeSample =
    let
        fineTuneXInitial =
            0
    in
    Pipeline.decode Sample
        |> Pipeline.hardcoded ""
        |> Pipeline.required decoderLength
        |> Pipeline.required decoderFinetune
        |> Pipeline.hardcoded fineTuneXInitial
        |> Pipeline.required decoderVolume
        |> Pipeline.required decoderLoop
        |> Pipeline.hardcoded []
        |> Bytes.andThen
            (\sample ->
                Bytes.succeed { sample | finetuneX = finetuneX sample.finetune }
            )


decoderSample : Instrument -> Bytes.Decoder Instrument
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
            (Pipeline.list instrument.sample.length Bytes.signedInt8
                |> Bytes.map (List.reverse >> List.map toFloat)
            )
        |> Bytes.andThen
            (\data ->
                let
                    sample =
                        instrument.sample

                    newSample =
                        { sample | data = data }
                in
                Bytes.succeed { instrument | sample = newSample }
            )


decoder : Int -> Bytes.Decoder Instrument
decoder index =
    Pipeline.decode Instrument
        |> Pipeline.required decoderName
        |> Pipeline.hardcoded "sample"
        |> Pipeline.required decodeSample
        |> Pipeline.hardcoded (InstrumentId index)
