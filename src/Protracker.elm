module Protracker exposing (Pattern, Protracker, decoder, getPatternByIndex, getRealPatternByIndex, getRowByIndex)

import Array exposing (Array)
import Bytes.Decode as Bytes
import Instrument exposing (Instrument)
import Note exposing (Note)
import Pipeline


type alias Row =
    List Note


type alias Pattern =
    List Row


type alias Protracker =
    { title : String
    , instruments : List Instrument
    , length : Int -- Number of song positions (ie. number of patterns played throughout the song). Legal values are 1..128.
    , patternTableLength : Int -- Historically set to 127, but can be safely ignored. Noisetracker uses this byte to indicate restart position - this has been made redundant by the 'Position Jump' effect.
    , patternTable : Array Int
    , signature : String
    , patterns : List Pattern
    }


getRealPatternByIndex : Int -> Protracker -> Maybe Pattern
getRealPatternByIndex index mod =
    let
        patternNumber =
            Array.get index mod.patternTable |> Maybe.withDefault 0
    in
    getPatternByIndex patternNumber mod.patterns


getPatternByIndex : Int -> List Pattern -> Maybe Pattern
getPatternByIndex i patterns =
    let
        patternsArray =
            patterns |> Array.fromList
    in
    Array.get i patternsArray


getRowByIndex : Int -> Pattern -> Maybe Row
getRowByIndex i pattern =
    let
        rowsArray =
            pattern |> Array.fromList
    in
    Array.get i rowsArray



-- DECODERS


decoderTitle : Bytes.Decoder String
decoderTitle =
    Bytes.string 20
        |> Bytes.map (String.replace (String.fromChar (Char.fromCode 0)) "")


channelsBySignature : String -> Int
channelsBySignature signature =
    case signature of
        "FLT8" ->
            8

        "2CHN" ->
            2

        "6CHN" ->
            6

        "8CHN" ->
            8

        "OCTA" ->
            8

        "10CH" ->
            10

        "12CH" ->
            12

        "14CH" ->
            14

        "16CH" ->
            16

        "18CH" ->
            18

        "20CH" ->
            20

        "22CH" ->
            22

        "24CH" ->
            24

        "26CH" ->
            26

        "28CH" ->
            28

        "30CH" ->
            30

        "32CH" ->
            32

        _ ->
            4


decoderPatternTable : Int -> Bytes.Decoder (Array Int)
decoderPatternTable patternTableLen =
    Pipeline.list patternTableLen Bytes.unsignedInt8 |> Bytes.map (List.reverse >> Array.fromList)


decoderSignature : Bytes.Decoder String
decoderSignature =
    Bytes.string 4


decoderRow : Int -> Bytes.Decoder Row
decoderRow channels =
    Pipeline.list channels Note.decoder |> Bytes.map List.reverse


decoderDivision : Int -> Bytes.Decoder Pattern
decoderDivision channels =
    Pipeline.list 64 (decoderRow channels) |> Bytes.map List.reverse


decoderPatterns : Protracker -> Bytes.Decoder Protracker
decoderPatterns song =
    let
        channels =
            channelsBySignature song.signature

        highestPattern =
            song.patternTable
                |> Array.toList
                |> List.maximum
                |> Maybe.map ((+) 1)
                |> Maybe.withDefault 0
    in
    Pipeline.list highestPattern (decoderDivision channels)
        |> Bytes.map List.reverse
        |> Bytes.andThen
            (\patterns ->
                Bytes.succeed
                    { song
                        | patterns = patterns
                    }
            )


decoderSamples : Protracker -> Bytes.Decoder Protracker
decoderSamples song =
    Pipeline.listCustom song.instruments Instrument.decoderSample
        |> Bytes.andThen
            (\instruments ->
                Bytes.succeed { song | instruments = instruments |> List.reverse }
            )


decoder : Bytes.Decoder Protracker
decoder =
    Pipeline.decode Protracker
        |> Pipeline.required decoderTitle
        |> Pipeline.required (Pipeline.listCustom (List.range 0 30) Instrument.decoder |> Bytes.map List.reverse)
        |> Pipeline.required Bytes.unsignedInt8
        |> Pipeline.required Bytes.unsignedInt8
        |> Pipeline.required (decoderPatternTable 128)
        |> Pipeline.required decoderSignature
        |> Pipeline.hardcoded []
        |> Bytes.andThen decoderPatterns
        |> Bytes.andThen decoderSamples
