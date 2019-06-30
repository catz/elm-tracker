module Protracker exposing (Pattern, PatternTable, ProTracker, Row, decoder, getPatternByIndex, getRealPatternByIndex, getRowByIndex)

import Array exposing (Array)
import Bytes.Decode as Bytes
import Instrument exposing (Instrument, Sample)
import Note exposing (Note)
import Pipeline


type alias Row =
    List Note


type alias Pattern =
    List Row


type alias PatternTable =
    Array Int


type alias ProTracker =
    { title : String
    , instruments : List Instrument
    , samples : List Sample
    , length : Int -- Number of song positions (ie. number of patterns played throughout the song). Legal values are 1..128.
    , patternTableLength : Int -- Historically set to 127, but can be safely ignored. Noisetracker uses this byte to indicate restart position - this has been made redundant by the 'Position Jump' effect.
    , patternTable : PatternTable
    , signature : String
    , patterns : List Pattern
    }


getRealPatternByIndex : Int -> ProTracker -> Maybe Pattern
getRealPatternByIndex index mod =
    let
        patternNumber =
            Array.get index mod.patternTable |> Maybe.withDefault 0
    in
    getPatternByIndex patternNumber mod.patterns


getPatternByIndex : Int -> List Pattern -> Maybe Pattern
getPatternByIndex idx patterns =
    if idx < 0 then
        Nothing

    else
        List.head <| List.drop idx patterns


getRowByIndex : Int -> Pattern -> Maybe Row
getRowByIndex idx pattern =
    if idx < 0 then
        Nothing

    else
        List.head <| List.drop idx pattern



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


decoderPatterns : ProTracker -> Bytes.Decoder ProTracker
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


decoderSamples : ProTracker -> Bytes.Decoder ProTracker
decoderSamples song =
    Pipeline.listCustom song.instruments Instrument.decoderSample
        |> Bytes.andThen
            (\samples ->
                Bytes.succeed { song | samples = samples |> List.reverse }
            )


decoderInstruments : Bytes.Decoder (List Instrument)
decoderInstruments =
    Pipeline.listCustom (List.range 0 30) Instrument.decoder
        |> Bytes.map List.reverse


decoder : Bytes.Decoder ProTracker
decoder =
    Pipeline.decode ProTracker
        |> Pipeline.required decoderTitle
        |> Pipeline.required decoderInstruments
        |> Pipeline.hardcoded []
        |> Pipeline.required Bytes.unsignedInt8
        |> Pipeline.required Bytes.unsignedInt8
        |> Pipeline.required (decoderPatternTable 128)
        |> Pipeline.required decoderSignature
        |> Pipeline.hardcoded []
        |> Bytes.andThen decoderPatterns
        |> Bytes.andThen decoderSamples
