module Note exposing (Note, decoder)

import Bitwise
import Bytes exposing (Endianness(..))
import Bytes.Decode as Bytes
import Effect exposing (Effect)
import Instrument exposing (InstrumentId(..))


type alias Note =
    { effect : Effect
    , instrumentId : Maybe InstrumentId
    , period : Maybe Int
    , time : Float
    }



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

                    periodValue =
                        if period == 0 then
                            Nothing

                        else
                            Just period

                    param =
                        Bitwise.and trackStepInfo 0xFF
                in
                Bytes.succeed (Note (Effect.create effect param) instrumentId periodValue 0)
            )
