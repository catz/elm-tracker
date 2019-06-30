module Noteperiod exposing (NotePeriodTable, Noteperiod, getFineTuneForPeriod, getSampleRateForPeriod, list, periodTable)

import Dict exposing (Dict)
import NoteName exposing (NoteName(..), Octave(..))


type alias Noteperiod =
    { period : Int
    , name : NoteName.Note
    , tune : List Int
    }


list : List Noteperiod
list =
    [ Noteperiod 856 (NoteName.Note C One) [ 907, 900, 894, 887, 881, 875, 868, 862, 856, 850, 844, 838, 832, 826, 820, 814 ]
    , Noteperiod 808 (NoteName.Note Cs One) [ 856, 850, 844, 838, 832, 826, 820, 814, 808, 802, 796, 791, 785, 779, 774, 768 ]
    , Noteperiod 762 (NoteName.Note D One) [ 808, 802, 796, 791, 785, 779, 774, 768, 762, 757, 752, 746, 741, 736, 730, 725 ]
    , Noteperiod 720 (NoteName.Note Ds One) [ 762, 757, 752, 746, 741, 736, 730, 725, 720, 715, 709, 704, 699, 694, 689, 684 ]
    , Noteperiod 678 (NoteName.Note E One) [ 720, 715, 709, 704, 699, 694, 689, 684, 678, 674, 670, 665, 660, 655, 651, 646 ]
    , Noteperiod 640 (NoteName.Note F One) [ 678, 675, 670, 665, 660, 655, 651, 646, 640, 637, 632, 628, 623, 619, 614, 610 ]
    , Noteperiod 604 (NoteName.Note Fs One) [ 640, 636, 632, 628, 623, 619, 614, 610, 604, 601, 597, 592, 588, 584, 580, 575 ]
    , Noteperiod 570 (NoteName.Note G One) [ 604, 601, 597, 592, 588, 584, 580, 575, 570, 567, 563, 559, 555, 551, 547, 543 ]
    , Noteperiod 538 (NoteName.Note Gs One) [ 570, 567, 563, 559, 555, 551, 547, 543, 538, 535, 532, 528, 524, 520, 516, 513 ]
    , Noteperiod 508 (NoteName.Note A One) [ 538, 535, 532, 528, 524, 520, 516, 513, 508, 505, 502, 498, 495, 491, 487, 484 ]
    , Noteperiod 480 (NoteName.Note As One) [ 508, 505, 502, 498, 494, 491, 487, 484, 480, 477, 474, 470, 467, 463, 460, 457 ]
    , Noteperiod 453 (NoteName.Note B One) [ 480, 477, 474, 470, 467, 463, 460, 457, 453, 450, 447, 444, 441, 437, 434, 431 ]
    , Noteperiod 428 (NoteName.Note C Two) [ 453, 450, 447, 444, 441, 437, 434, 431, 428, 425, 422, 419, 416, 413, 410, 407 ]
    , Noteperiod 404 (NoteName.Note Cs Two) [ 428, 425, 422, 419, 416, 413, 410, 407, 404, 401, 398, 395, 392, 390, 387, 384 ]
    , Noteperiod 381 (NoteName.Note D Two) [ 404, 401, 398, 395, 392, 390, 387, 384, 381, 379, 376, 373, 370, 368, 365, 363 ]
    , Noteperiod 360 (NoteName.Note Ds Two) [ 381, 379, 376, 373, 370, 368, 365, 363, 360, 357, 355, 352, 350, 347, 345, 342 ]
    , Noteperiod 339 (NoteName.Note E Two) [ 360, 357, 355, 352, 350, 347, 345, 342, 339, 337, 335, 332, 330, 328, 325, 323 ]
    , Noteperiod 320 (NoteName.Note F Two) [ 339, 337, 335, 332, 330, 328, 325, 323, 320, 318, 316, 314, 312, 309, 307, 305 ]
    , Noteperiod 302 (NoteName.Note Fs Two) [ 320, 318, 316, 314, 312, 309, 307, 305, 302, 300, 298, 296, 294, 292, 290, 288 ]
    , Noteperiod 285 (NoteName.Note G Two) [ 302, 300, 298, 296, 294, 292, 290, 288, 285, 284, 282, 280, 278, 276, 274, 272 ]
    , Noteperiod 269 (NoteName.Note Gs Two) [ 285, 284, 282, 280, 278, 276, 274, 272, 269, 268, 266, 264, 262, 260, 258, 256 ]
    , Noteperiod 254 (NoteName.Note A Two) [ 269, 268, 266, 264, 262, 260, 258, 256, 254, 253, 251, 249, 247, 245, 244, 242 ]
    , Noteperiod 240 (NoteName.Note As Two) [ 254, 253, 251, 249, 247, 245, 244, 242, 240, 239, 237, 235, 233, 232, 230, 228 ]
    , Noteperiod 226 (NoteName.Note B Two) [ 240, 238, 237, 235, 233, 232, 230, 228, 226, 225, 224, 222, 220, 219, 217, 216 ]
    , Noteperiod 214 (NoteName.Note C Three) [ 226, 225, 223, 222, 220, 219, 217, 216, 214, 213, 211, 209, 208, 206, 205, 204 ]
    , Noteperiod 202 (NoteName.Note Cs Three) [ 214, 212, 211, 209, 208, 206, 205, 203, 202, 201, 199, 198, 196, 195, 193, 192 ]
    , Noteperiod 190 (NoteName.Note D Three) [ 202, 200, 199, 198, 196, 195, 193, 192, 190, 189, 188, 187, 185, 184, 183, 181 ]
    , Noteperiod 180 (NoteName.Note Ds Three) [ 190, 189, 188, 187, 185, 184, 183, 181, 180, 179, 177, 176, 175, 174, 172, 171 ]
    , Noteperiod 170 (NoteName.Note E Three) [ 180, 179, 177, 176, 175, 174, 172, 171, 170, 169, 167, 166, 165, 164, 163, 161 ]
    , Noteperiod 160 (NoteName.Note F Three) [ 170, 169, 167, 166, 165, 164, 163, 161, 160, 159, 158, 157, 156, 155, 154, 152 ]
    , Noteperiod 151 (NoteName.Note Fs Three) [ 160, 159, 158, 157, 156, 155, 154, 152, 151, 150, 149, 148, 147, 146, 145, 144 ]
    , Noteperiod 143 (NoteName.Note G Three) [ 151, 150, 149, 148, 147, 146, 145, 144, 143, 142, 141, 140, 139, 138, 137, 136 ]
    , Noteperiod 135 (NoteName.Note Gs Three) [ 143, 142, 141, 140, 139, 138, 137, 136, 135, 134, 133, 132, 131, 130, 129, 128 ]
    , Noteperiod 127 (NoteName.Note A Three) [ 135, 134, 133, 132, 131, 130, 129, 128, 127, 126, 125, 125, 124, 123, 122, 121 ]
    , Noteperiod 120 (NoteName.Note As Three) [ 127, 126, 125, 125, 123, 123, 122, 121, 120, 119, 118, 118, 117, 116, 115, 114 ]
    , Noteperiod 113 (NoteName.Note B Three) [ 120, 119, 118, 118, 117, 116, 115, 114, 113, 113, 112, 111, 110, 109, 109, 108 ]
    ]


type alias NotePeriodTable =
    Dict Int Noteperiod


periodTable : NotePeriodTable
periodTable =
    list |> List.map (\v -> ( v.period, v )) |> Dict.fromList


getFineTuneForPeriod : Int -> Int -> Int
getFineTuneForPeriod period finetune =
    Dict.get period periodTable
        |> Maybe.map
            (\noteperiod ->
                let
                    centerTune =
                        8

                    tune =
                        centerTune + finetune
                in
                if tune >= 0 then
                    noteperiod.tune |> List.drop 4 |> List.head |> Maybe.withDefault period

                else
                    period
            )
        |> Maybe.withDefault period



-- Amiga Frequency


amiga_palfrequency : Int
amiga_palfrequency =
    7093790


amiga_frequency_half : Float
amiga_frequency_half =
    toFloat amiga_palfrequency / 2


getSampleRateForPeriod : Int -> Float
getSampleRateForPeriod period =
    amiga_frequency_half / toFloat period
