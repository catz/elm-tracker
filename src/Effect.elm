module Effect exposing (Effect(..), SubEffect(..), create, format)

import Bitwise
import Util


type Effect
    = None
    | Arpeggio Int
    | SlideUp Int
    | SlideDown Int
    | TonePortamento Int
    | Vibrato Int
    | TonePortamentoVolumeSlide Int
    | VibratoVolumeSlide Int
    | Tremolo Int
    | SampleOffset Int
    | VolumeSlide Int
    | PositionJump Int
    | SetVolume Int
    | PatternBreak Int
    | SetSpeed Int
    | Other SubEffect


type SubEffect
    = AmigaLed Int
    | FinePortamentoUp Int
    | FinePortamentoDown Int
    | GlissandoControl Int
    | VibratoControl Int
    | SetNoteFineTune Int
    | PatternLoop Int
    | TremolloControl Int
    | SetNotePanningPossition Int
    | ReTriggerNote Int
    | FineVolumeSlideUp Int
    | FineVolumeSlideDown Int
    | NoteCut Int
    | NoteDelay Int
    | PatternDelay Int
    | FunkIt


create : Int -> Int -> Effect
create effect param =
    case effect of
        0 ->
            if param == 0 then
                None

            else
                Arpeggio param

        1 ->
            SlideUp param

        2 ->
            SlideDown param

        3 ->
            TonePortamento param

        4 ->
            Vibrato param

        5 ->
            TonePortamentoVolumeSlide param

        6 ->
            VibratoVolumeSlide param

        7 ->
            Tremolo param

        9 ->
            SampleOffset param

        10 ->
            VolumeSlide param

        11 ->
            PositionJump param

        12 ->
            SetVolume param

        13 ->
            PatternBreak param

        14 ->
            let
                subEffect =
                    param |> Bitwise.shiftRightBy 4

                subValue =
                    param |> Bitwise.and 0x0F
            in
            case subEffect of
                0 ->
                    Other (AmigaLed subValue)

                1 ->
                    Other (FinePortamentoUp subValue)

                2 ->
                    Other (FinePortamentoDown subValue)

                3 ->
                    Other (GlissandoControl subValue)

                4 ->
                    Other (VibratoControl subValue)

                5 ->
                    Other (SetNoteFineTune subValue)

                6 ->
                    Other (PatternLoop subValue)

                7 ->
                    Other (TremolloControl subValue)

                8 ->
                    Other (SetNotePanningPossition subValue)

                9 ->
                    Other (ReTriggerNote subValue)

                10 ->
                    Other (FineVolumeSlideUp subValue)

                11 ->
                    Other (FineVolumeSlideDown subValue)

                12 ->
                    Other (NoteCut subValue)

                13 ->
                    Other (NoteDelay subValue)

                14 ->
                    Other (PatternDelay subValue)

                _ ->
                    Other FunkIt

        15 ->
            SetSpeed param

        _ ->
            None


value : Effect -> ( Int, Int )
value effect =
    case effect of
        None ->
            ( 0, 0 )

        Arpeggio param ->
            ( 0, param )

        SlideUp param ->
            ( 1, param )

        SlideDown param ->
            ( 2, param )

        TonePortamento param ->
            ( 3, param )

        Vibrato param ->
            ( 4, param )

        TonePortamentoVolumeSlide param ->
            ( 5, param )

        VibratoVolumeSlide param ->
            ( 6, param )

        Tremolo param ->
            ( 7, param )

        SampleOffset param ->
            ( 9, param )

        VolumeSlide param ->
            ( 10, param )

        PositionJump param ->
            ( 11, param )

        SetVolume param ->
            ( 12, param )

        PatternBreak param ->
            ( 13, param )

        SetSpeed param ->
            ( 15, param )

        Other subeffect ->
            case subeffect of
                AmigaLed param ->
                    ( 224, param )

                FinePortamentoUp param ->
                    ( 225, param )

                FinePortamentoDown param ->
                    ( 226, param )

                GlissandoControl param ->
                    ( 227, param )

                VibratoControl param ->
                    ( 228, param )

                SetNoteFineTune param ->
                    ( 229, param )

                PatternLoop param ->
                    ( 230, param )

                TremolloControl param ->
                    ( 231, param )

                SetNotePanningPossition param ->
                    ( 232, param )

                ReTriggerNote param ->
                    ( 233, param )

                FineVolumeSlideUp param ->
                    ( 234, param )

                FineVolumeSlideDown param ->
                    ( 235, param )

                NoteCut param ->
                    ( 236, param )

                NoteDelay param ->
                    ( 237, param )

                PatternDelay param ->
                    ( 238, param )

                FunkIt ->
                    ( 240, 0 )


format : Effect -> String
format effect =
    let
        ( effectValue, effectParam ) =
            value effect
    in
    case effect of
        None ->
            "..."

        Other _ ->
            Util.formatHex effectValue 2 ' ' ++ Util.formatHex effectParam 1 '0'

        _ ->
            Util.formatHex effectValue 1 ' ' ++ Util.formatHex effectParam 2 '0'
