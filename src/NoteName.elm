module NoteName exposing (Note(..), NoteName(..), Octave(..), octaveToInt, toString)


type Octave
    = One
    | Two
    | Three


type NoteName
    = C
    | Cs
    | D
    | Ds
    | E
    | F
    | Fs
    | G
    | Gs
    | A
    | As
    | B


type Note
    = Note NoteName Octave


octaveToInt : Octave -> Int
octaveToInt octave =
    case octave of
        One ->
            1

        Two ->
            2

        Three ->
            3


toString : Note -> String
toString (Note name octave) =
    let
        octaveString =
            octaveToInt octave |> String.fromInt

        nameString =
            case name of
                C ->
                    "C-"

                Cs ->
                    "C#"

                D ->
                    "D-"

                Ds ->
                    "D#"

                E ->
                    "E-"

                F ->
                    "F-"

                Fs ->
                    "F#"

                G ->
                    "G-"

                Gs ->
                    "G#"

                A ->
                    "A-"

                As ->
                    "A#"

                B ->
                    "B-"
    in
    nameString ++ octaveString
