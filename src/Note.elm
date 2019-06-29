module Note exposing (Note, decoder, toStrings)

import Bitwise
import Bytes exposing (Endianness(..))
import Bytes.Decode as Bytes
import Dict
import Instrument exposing (InstrumentId(..))
import NoteName
import Noteperiod
import Util


type alias Note =
    { effect : Int
    , instrumentId : Maybe InstrumentId
    , param : Int
    , period : Int
    , time : Float
    }


toStrings : Note -> { noteString : String, instrumentString : String, effectString : String }
toStrings note =
    let
        noteString =
            Dict.get note.period Noteperiod.periodTable
                |> Maybe.map (.name >> NoteName.toString)
                |> Maybe.withDefault "---"

        instrumentString =
            Instrument.toString note.instrumentId

        effectString =
            if note.effect > 15 then
                Util.formatHexExtended note.effect 1 ' '

            else if note.effect == 0 then
                "."

            else
                Util.formatHex note.effect 1 ' '

        paramString =
            if note.effect > 0 then
                Util.formatHex note.param 2 '0'

            else
                ".."
    in
    { noteString = noteString, instrumentString = instrumentString, effectString = effectString ++ paramString }



-- DECODERS


decoder : Bytes.Decoder Note
decoder =
    Bytes.unsignedInt32 BE
        |> Bytes.andThen
            (\trackStepInfo ->
                let
                    period =
                        trackStepInfo |> Bitwise.shiftRightBy 16 |> Bitwise.and 0x0FFF

                    effect =
                        trackStepInfo |> Bitwise.shiftRightBy 8 |> Bitwise.and 0x0F

                    instrumentIdNumber =
                        Bitwise.or (Bitwise.shiftRightBy 24 trackStepInfo |> Bitwise.and 0xF0) (Bitwise.shiftRightBy 12 trackStepInfo |> Bitwise.and 0x0F)

                    instrumentId =
                        if instrumentIdNumber == 0 then
                            Nothing

                        else
                            Just (InstrumentId instrumentIdNumber)

                    param =
                        Bitwise.and trackStepInfo 0xFF
                in
                Bytes.succeed (Note effect instrumentId param period 0)
            )
