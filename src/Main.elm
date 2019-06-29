port module Main exposing (main, update)

import Array exposing (Array)
import Browser
import Browser.Dom exposing (Viewport, getViewportOf, setViewportOf)
import Bytes exposing (Bytes)
import Bytes.Decode as Bytes
import File exposing (File)
import File.Select as Select
import Html exposing (Attribute, Html, br, button, div, h2, i, input, li, option, select, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (checked, class, classList, disabled, id, name, selected, size, type_)
import Html.Events exposing (onClick, onMouseDown, onMouseUp)
import Html.Keyed as Keyed
import Http
import Instrument exposing (Instrument, InstrumentId(..))
import Json.Decode as Decode
import Json.Encode exposing (Value)
import Note exposing (Note)
import NoteName exposing (Octave(..))
import Noteperiod exposing (NotePeriodTable, Noteperiod)
import Protracker exposing (Pattern, Protracker)
import Task
import WebAudio


type alias Model =
    { mod : Maybe Protracker
    , isPlaying : Bool
    , currentPatternIndex : Int
    , error : Maybe String
    , currentOctave : Octave
    , currentPianoInstrumentIndex : Int
    , currentPianoInstrument : Maybe Instrument
    , now : Float
    , playPianoNow : Float
    , currentPianoPeriod : Maybe Int
    , isPianoShown : Bool
    , activeStepIndex : Int
    , stepTime : Float
    , currentRow : List Note
    , muteChannels : Array Bool
    }


type alias Flags =
    Decode.Value


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { mod = Nothing
      , error = Nothing
      , isPlaying = False
      , isPianoShown = False
      , currentPatternIndex = 0
      , currentOctave = One
      , currentPianoInstrumentIndex = 0
      , currentPianoInstrument = Nothing
      , currentPianoPeriod = Nothing
      , now = 0
      , playPianoNow = 0
      , activeStepIndex = 0
      , stepTime = 0
      , currentRow = []
      , muteChannels = Array.empty
      }
    , getModule
    )



-- Ports


port renderSample : Value -> Cmd msg



-- Feel free to change Messages and update part


type Msg
    = NoOp
    | GotData (Result Http.Error Protracker)
    | PrevPattern
    | NextPattern
    | FirstPattern
    | LastPattern
    | SelectPattern Int
    | ModuleRequested
    | ModuleSelected File
    | ModuleLoaded Bytes
    | OctaveUp
    | OctaveDown
    | SelectInstrument Instrument
    | PlayInstrument Int
    | StopInstrument
    | AssetLoaded (List WebAudio.Url)
    | Tick WebAudio.Time
    | TogglePiano
    | TogglePlay
    | ToggleMute Int
    | SetNextStep (Result Browser.Dom.Error Viewport)
    | SetFirstStep


jumpToTop : Cmd Msg
jumpToTop =
    setViewportOf "patterns" 0 0
        |> Task.attempt (\_ -> SetFirstStep)


jumpToNextLine : Cmd Msg
jumpToNextLine =
    getViewportOf "patterns"
        |> Task.attempt SetNextStep


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotData result ->
            case result of
                Ok mod ->
                    ( setModule mod model
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | error = Just "Error" }, Cmd.none )

        PrevPattern ->
            ( { model | currentPatternIndex = model.currentPatternIndex - 1 }, jumpToTop )

        NextPattern ->
            ( { model | currentPatternIndex = model.currentPatternIndex + 1 }, jumpToTop )

        FirstPattern ->
            ( { model | currentPatternIndex = 0 }, jumpToTop )

        LastPattern ->
            case model.mod of
                Nothing ->
                    ( model, Cmd.none )

                Just mod ->
                    ( { model | currentPatternIndex = mod.length - 1 }, jumpToTop )

        SelectPattern step ->
            case model.mod of
                Nothing ->
                    ( model, Cmd.none )

                Just _ ->
                    ( { model | currentPatternIndex = step }, Cmd.none )

        ModuleRequested ->
            ( model
            , Select.file [ "application/mod" ] ModuleSelected
            )

        ModuleSelected file ->
            ( model
            , Task.perform ModuleLoaded (File.toBytes file)
            )

        ModuleLoaded content ->
            let
                modLoaded =
                    Bytes.decode Protracker.decoder content
            in
            case modLoaded of
                Just mod ->
                    ( setModule mod model, Cmd.none )

                Nothing ->
                    ( { model | error = Just "Error" }, Cmd.none )

        OctaveUp ->
            case model.currentOctave of
                One ->
                    ( { model | currentOctave = Two }, Cmd.none )

                Two ->
                    ( { model | currentOctave = Three }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        OctaveDown ->
            case model.currentOctave of
                Three ->
                    ( { model | currentOctave = Two }, Cmd.none )

                Two ->
                    ( { model | currentOctave = One }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SelectInstrument instrument ->
            ( { model | currentPianoInstrument = Just instrument }, Cmd.none )

        PlayInstrument period ->
            ( { model | currentPianoPeriod = Just period, playPianoNow = model.now }, Cmd.none )

        StopInstrument ->
            ( { model | currentPianoPeriod = Nothing }, Cmd.none )

        AssetLoaded assets ->
            ( model, Cmd.none )

        Tick (WebAudio.Time audioTime) ->
            if model.isPlaying then
                let
                    ticksPerStep =
                        4

                    tickTime =
                        0.02

                    delta =
                        audioTime - model.now

                    a =
                        Debug.log "delta" delta

                    nextStep =
                        (model.stepTime + delta) >= (ticksPerStep * tickTime)

                    a1 =
                        Debug.log "<" (audioTime < model.now)

                    stepTime =
                        if nextStep then
                            0

                        else
                            model.stepTime + delta

                    ( activeStepIndex, currentPatternIndex, cmd ) =
                        if nextStep then
                            if model.activeStepIndex + 1 > 63 then
                                ( 0
                                , model.currentPatternIndex + 1
                                , jumpToTop
                                )

                            else
                                ( model.activeStepIndex + 1, model.currentPatternIndex, jumpToNextLine )

                        else
                            ( model.activeStepIndex, model.currentPatternIndex, Cmd.none )

                    currentRow =
                        model.mod
                            |> Maybe.andThen (Protracker.getRealPatternByIndex model.currentPatternIndex)
                            |> Maybe.andThen (Protracker.getRowByIndex activeStepIndex)
                            |> Maybe.withDefault []

                    previousRow =
                        if List.length model.currentRow == 0 then
                            currentRow

                        else
                            model.currentRow

                    updatedRow =
                        List.map2
                            (\prev curr ->
                                if (curr.period /= 0 && curr.instrumentId /= Nothing) && (curr.time /= audioTime) then
                                    { curr | time = audioTime }

                                else
                                    prev
                            )
                            previousRow
                            currentRow
                in
                ( { model
                    | now = audioTime
                    , stepTime = stepTime
                    , currentPatternIndex = currentPatternIndex
                    , currentRow = updatedRow
                  }
                , cmd
                )

            else
                ( { model | now = audioTime }, Cmd.none )

        TogglePlay ->
            ( { model | isPlaying = not model.isPlaying, stepTime = 0 }, Cmd.none )

        TogglePiano ->
            ( { model | isPianoShown = not model.isPianoShown }, Cmd.none )

        ToggleMute channel ->
            let
                muteChannels =
                    Array.indexedMap
                        (\i state ->
                            if i == channel then
                                not state

                            else
                                state
                        )
                        model.muteChannels
            in
            ( { model
                | muteChannels = muteChannels
              }
            , Cmd.none
            )

        SetNextStep result ->
            case result of
                Ok viewport ->
                    let
                        cmd =
                            setViewportOf "patterns" 0 (viewport.viewport.y + 30)
                                |> Task.attempt (\_ -> NoOp)
                    in
                    ( { model | activeStepIndex = model.activeStepIndex + 1 }, cmd )

                Err _ ->
                    ( model, Cmd.none )

        SetFirstStep ->
            ( { model | activeStepIndex = 0 }, Cmd.none )


setModule : Protracker -> Model -> Model
setModule mod model =
    { model
        | mod = Just mod
        , currentPatternIndex = 0
        , currentPianoInstrument = mod.instruments |> List.filter (\i -> List.length i.sample.data > 0) |> List.head
        , currentPianoPeriod = Nothing
        , now = 0
        , playPianoNow = 0
        , isPlaying = False
        , activeStepIndex = 0
        , error = Nothing
        , muteChannels =
            List.head mod.patterns
                |> Maybe.andThen List.head
                |> Maybe.map List.length
                |> Maybe.map (\l -> Array.initialize l (always False))
                |> Maybe.withDefault Array.empty
    }


viewNote : Note -> Html Msg
viewNote note =
    let
        { noteString, instrumentString, effectString } =
            Note.toStrings note
    in
    div [ class "pattern-table" ]
        [ span [ class "note" ] [ text noteString ]
        , span [ class "instrument" ] [ text instrumentString ]
        , span [ class "command" ] [ text effectString ]
        ]


viewPattern : Array Bool -> Int -> Pattern -> Html Msg
viewPattern muteChannels activeStepIndex pattern =
    let
        viewMuteCell channel state =
            th []
                [ input [ type_ "checkbox", checked state, onClick <| ToggleMute channel ] [] ]

        emptyRows =
            List.range 0 4
                |> List.map (\_ -> tr [ class "empty-row" ] (List.range 0 4 |> List.map (\_ -> td [] [])))
    in
    table [ class "patterns" ]
        [ thead []
            [ tr [] ([ th [] [ text "Mute" ] ] ++ (muteChannels |> Array.toList |> List.indexedMap viewMuteCell))
            ]
        , tbody [ id "patterns" ]
            (emptyRows
                ++ List.indexedMap
                    (\i row ->
                        tr [ id <| "row-" ++ String.fromInt i, classList [ ( "active", i == activeStepIndex ) ] ]
                            (td []
                                [ span []
                                    [ text
                                        (i
                                            |> String.fromInt
                                            |> String.padLeft 2 '0'
                                        )
                                    ]
                                ]
                                :: List.map (\note -> td [] [ viewNote note ]) row
                            )
                    )
                    pattern
                ++ emptyRows
            )
        ]


viewModule : Int -> Int -> Array Int -> Html Msg
viewModule length currentPatternIndex patternTable =
    select [ size 5 ]
        (List.range 0 (length - 1)
            |> List.map
                (\i ->
                    option [ onClick <| SelectPattern i, name <| "pattern-" ++ String.fromInt i, selected (i == currentPatternIndex) ]
                        [ text <|
                            (String.fromInt (i + 1) |> String.padLeft 2 '0')
                                ++ ":"
                                ++ (Array.get i patternTable
                                        |> Maybe.withDefault 0
                                        |> String.fromInt
                                        |> String.padLeft 2 '0'
                                   )
                        ]
                )
        )


viewOctaveSelector : Octave -> Html Msg
viewOctaveSelector currentOctave =
    div []
        [ span [ class "current-octave" ] [ text <| "Octave: " ++ (NoteName.octaveToInt currentOctave |> String.fromInt) ]
        , button [ onClick OctaveDown ] [ text "<" ]
        , button [ onClick OctaveUp ] [ text ">" ]
        ]


viewInstruments : Int -> List Instrument -> Html Msg
viewInstruments currentPianoInstrumentIndex instruments =
    ul []
        (List.map
            (\instrument ->
                li []
                    [ text <| instrument.name
                    , if List.length instrument.sample.data > 0 then
                        button [ onClick (SelectInstrument instrument) ] [ text "select" ]

                      else
                        text ""
                    ]
            )
            instruments
        )


instrumentGainNode : Instrument -> WebAudio.Props
instrumentGainNode instrument =
    let
        volume =
            100 * toFloat instrument.sample.volume / 64
    in
    WebAudio.Gain
        { gain =
            WebAudio.Constant (volume / 100)
        }


instrumentBufferNode : InstrumentId -> Int -> Float -> WebAudio.Props
instrumentBufferNode (InstrumentId instrumentId) period time =
    let
        sampleRate =
            Noteperiod.getSampleRateForPeriod period

        -- TODO audioContext.sampleRate = 48000
        initialPlaybackRate =
            sampleRate / 48000
    in
    WebAudio.BufferSource
        { buffer = WebAudio.Url (instrumentId |> (+) -1 |> String.fromInt)
        , detune = 0
        , startTime = WebAudio.Time time
        , stopTime = Nothing
        , playbackRate = initialPlaybackRate
        }


instrumentPianoNodes : Model -> List WebAudio.Node
instrumentPianoNodes model =
    case model.mod of
        Nothing ->
            []

        Just mod ->
            case Maybe.map2 Tuple.pair model.currentPianoInstrument model.currentPianoPeriod of
                Just ( instrument, period ) ->
                    [ { id = WebAudio.NodeId "pianoGain"
                      , output = [ WebAudio.Output (WebAudio.NodeId "final") ]
                      , props = instrumentGainNode instrument
                      }
                    , { id = WebAudio.NodeId "piano"
                      , output = [ WebAudio.Output (WebAudio.NodeId "pianoGain") ]
                      , props = instrumentBufferNode instrument.id period model.playPianoNow
                      }
                    ]

                _ ->
                    []


audioGraph : Model -> WebAudio.Graph
audioGraph model =
    let
        currentRowNodes =
            if model.isPlaying then
                List.indexedMap
                    (\channel note ->
                        let
                            isMuted =
                                Array.get channel model.muteChannels |> Maybe.withDefault False
                        in
                        Maybe.andThen
                            (\instrumentId ->
                                if note.period > 0 && not isMuted then
                                    Just
                                        { id = WebAudio.NodeId ("channel" ++ String.fromInt channel)
                                        , output = [ WebAudio.Output (WebAudio.NodeId "final") ]
                                        , props =
                                            instrumentBufferNode instrumentId note.period note.time
                                        }

                                else
                                    Nothing
                            )
                            note.instrumentId
                    )
                    model.currentRow
                    |> List.filterMap identity

            else
                []

        nodes =
            [ { id = WebAudio.NodeId "final"
              , output = WebAudio.output
              , props = WebAudio.Gain { gain = WebAudio.Constant 1 }
              }
            ]
                ++ instrumentPianoNodes model
                ++ currentRowNodes
    in
    nodes


samples : List Instrument -> List ( WebAudio.Url, List Float )
samples instruments =
    instruments
        |> List.filterMap
            (\{ sample, id } ->
                case id of
                    InstrumentId instrumentId ->
                        if List.length sample.data > 0 then
                            Just ( WebAudio.Url (instrumentId |> String.fromInt), sample.data )

                        else
                            Nothing
            )


viewAudio : Model -> Html Msg
viewAudio model =
    case model.mod of
        Nothing ->
            text ""

        Just mod ->
            Keyed.node "div"
                []
                [ ( mod.title
                  , WebAudio.toHtml
                        { graph =
                            audioGraph model
                        , assets = []
                        , onProgress = AssetLoaded
                        , onTick = Tick
                        , samples =
                            samples mod.instruments
                        }
                  )
                ]


viewPianoKey : Noteperiod -> Html Msg
viewPianoKey noteperiod =
    let
        note =
            noteperiod.name

        className =
            case note of
                NoteName.Note noteName _ ->
                    case noteName of
                        NoteName.C ->
                            "white c"

                        NoteName.Cs ->
                            "black cs"

                        NoteName.D ->
                            "white d"

                        NoteName.Ds ->
                            "black ds"

                        NoteName.E ->
                            "white e"

                        NoteName.F ->
                            "white f"

                        NoteName.Fs ->
                            "black fs"

                        NoteName.G ->
                            "white g"

                        NoteName.Gs ->
                            "black gs"

                        NoteName.A ->
                            "white a"

                        NoteName.As ->
                            "black as"

                        NoteName.B ->
                            "white b"
    in
    li [ class className, onMouseDown <| PlayInstrument noteperiod.period, onMouseUp StopInstrument ] []


viewPiano : Maybe Instrument -> Html Msg
viewPiano instrument =
    case instrument of
        Just i ->
            div [ id "piano" ]
                [ ul [ class "set" ]
                    (List.map viewPianoKey Noteperiod.list)
                ]

        Nothing ->
            text ""


view : Model -> Html Msg
view model =
    case model.error of
        Just err ->
            div []
                [ h2 [] [ text err ]
                , button [ onClick ModuleRequested ] [ text "Select" ]
                ]

        Nothing ->
            case model.mod of
                Nothing ->
                    h2 [] [ text "loading" ]

                Just mod ->
                    case Protracker.getRealPatternByIndex model.currentPatternIndex mod of
                        Nothing ->
                            h2 [] [ text "pattern not found" ]

                        Just pattern ->
                            let
                                piano =
                                    if model.isPianoShown then
                                        div []
                                            [ viewPiano model.currentPianoInstrument
                                            , br [] []

                                            --                                            , viewOctaveSelector model.currentOctave
                                            --                                            , br [] []
                                            , viewInstruments model.currentPianoInstrumentIndex mod.instruments
                                            , br [] []
                                            ]

                                    else
                                        text ""
                            in
                            div []
                                [ h2 [] [ text mod.title ]
                                , viewAudio model
                                , button [ onClick ModuleRequested ] [ text "Select" ]
                                , button [ onClick TogglePlay ] [ text "Toggle Play" ]
                                , viewModule mod.length model.currentPatternIndex mod.patternTable
                                , br [] []
                                , span [] [ text <| "Song length: " ++ String.fromInt mod.length ]
                                , br [] []
                                , button [ onClick FirstPattern ] [ text "<<" ]
                                , button [ onClick PrevPattern, disabled <| model.currentPatternIndex == 0 ] [ text "<" ]
                                , button [ onClick NextPattern, disabled <| model.currentPatternIndex == (mod.length - 1) ] [ text ">" ]
                                , button [ onClick LastPattern ] [ text ">>" ]
                                , viewPattern model.muteChannels model.activeStepIndex pattern
                                , br [] []
                                , button [ onClick TogglePiano ] [ text "Toggle Piano" ]
                                , piano
                                ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []



-- Ports to JS
-- HTTP


getModule : Cmd Msg
getModule =
    Http.get
        { --        url = "/mods/ITSMYLIF.MOD"
          --          url = "/mods/u_score.mod"
          url = "/mods/lasttrip.mod"
        , expect = Http.expectBytes GotData Protracker.decoder
        }
