port module Main exposing (main, update)

import Array exposing (Array)
import Browser
import Browser.Dom exposing (Viewport, setViewportOf)
import Browser.Events
import Bytes exposing (Bytes)
import Bytes.Decode as Bytes
import Dict
import Effect
import File exposing (File)
import File.Select as Select
import Html exposing (Attribute, Html, br, button, div, h2, h3, i, input, li, option, select, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (checked, class, classList, disabled, id, name, selected, size, type_)
import Html.Events exposing (onClick, onMouseDown, onMouseUp)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Http
import Instrument exposing (Instrument, InstrumentId(..), Sample)
import Json.Decode as Decode
import Json.Encode exposing (Value)
import List.Extra as LE
import Note exposing (Note)
import NoteName exposing (Octave(..))
import Noteperiod exposing (NotePeriodTable, Noteperiod)
import Process
import Protracker exposing (Pattern, PatternTable, ProTracker, Row)
import Song exposing (Song, State)
import Task
import Time
import WebAudio


type alias Model =
    { song : Maybe Song
    , error : Maybe String
    , isPlaying : Bool
    , currentOctave : Octave
    , currentPianoInstrument : Maybe Instrument
    , currentPianoPeriod : Maybe Int
    , audioContextTime : Float
    , playPianoNow : Float
    , isPianoShown : Bool
    , samplesLoaded : Bool
    , instruments : Array Instrument
    , currentState : Song.State
    }


type alias Flags =
    Decode.Value


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { song = Nothing
      , error = Nothing
      , isPlaying = False
      , isPianoShown = False
      , currentOctave = One
      , currentPianoInstrument = Nothing
      , currentPianoPeriod = Nothing
      , audioContextTime = 0
      , playPianoNow = 0
      , samplesLoaded = False
      , instruments = Array.empty
      , currentState = Song.State 0 0
      }
    , getModule "/mods/necros-the_traveler.mod"
    )



-- Ports


port renderSample : Value -> Cmd msg



-- Feel free to change Messages and update part


type Msg
    = NoOp
    | GotData (Result Http.Error ProTracker)
    | PrevPattern
    | NextPattern
    | FirstPattern
    | LastPattern
    | SelectPattern Int
    | ModuleRequested
    | ModuleSelected File
    | ModuleLoaded Bytes
    | SelectInstrument (Maybe Instrument)
    | PlayInstrument Int
    | StopInstrument
    | AssetLoaded (List WebAudio.Url)
    | AudioContextTick WebAudio.Time
    | PlayTick Time.Posix
    | TogglePiano
    | TogglePlay
    | ToggleMute Int
    | SetRowPosition Time.Posix
    | LoadModule String


setRowPosition : Song.State -> Cmd Msg
setRowPosition currentState =
    case currentState of
        Song.State _ processingPatternPosition ->
            setViewportOf "patterns" 0 (30 * toFloat processingPatternPosition)
                |> Task.attempt (\_ -> NoOp)


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotData result ->
            case result of
                Ok mod ->
                    let
                        song =
                            Song.create mod
                    in
                    ( { model
                        | song = Just song
                        , samplesLoaded = False
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | error = Just "Error" }, Cmd.none )

        PrevPattern ->
            case model.currentState of
                Song.State processingSongPosition _ ->
                    if processingSongPosition > 0 then
                        let
                            newState =
                                Song.State (processingSongPosition - 1) 0
                        in
                        ( { model | currentState = newState }, setRowPosition newState )

                    else
                        ( model, Cmd.none )

        NextPattern ->
            case model.song of
                Just song ->
                    case model.currentState of
                        Song.State processingSongPosition _ ->
                            if processingSongPosition < song.mod.length then
                                let
                                    newState =
                                        Song.State (processingSongPosition + 1) 0
                                in
                                ( { model | currentState = newState }, setRowPosition newState )

                            else
                                ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        FirstPattern ->
            ( { model | currentState = Song.State 0 0 }, setRowPosition <| Song.State 0 0 )

        LastPattern ->
            case model.song of
                Just song ->
                    case model.currentState of
                        Song.State _ processingPatternPosition ->
                            let
                                newState =
                                    Song.State (song.mod.length - 1) processingPatternPosition
                            in
                            ( { model | currentState = newState }, setRowPosition newState )

                Nothing ->
                    ( model, Cmd.none )

        SelectPattern pos ->
            ( { model | currentState = Song.State pos 0 }, setRowPosition <| Song.State pos 0 )

        ModuleRequested ->
            ( { model | error = Nothing }
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
                    ( { model
                        | song = Just <| Song.create mod
                        , samplesLoaded = False
                        , currentState = Song.State 0 0
                      }
                    , setRowPosition <| Song.State 0 0
                    )

                Nothing ->
                    ( { model | error = Just "Error" }, Cmd.none )

        SelectInstrument instrument ->
            ( { model | currentPianoInstrument = instrument }, Cmd.none )

        PlayInstrument period ->
            ( { model | currentPianoPeriod = Just period, playPianoNow = model.audioContextTime }, Cmd.none )

        StopInstrument ->
            ( { model | currentPianoPeriod = Nothing }, Cmd.none )

        AssetLoaded assets ->
            ( { model | samplesLoaded = True }, Cmd.none )

        AudioContextTick (WebAudio.Time audioTime) ->
            ( { model | audioContextTime = audioTime }, Cmd.none )

        TogglePlay ->
            case model.song of
                Just song ->
                    let
                        isPlaying =
                            not model.isPlaying

                        newSong =
                            if isPlaying then
                                { song
                                    | scheduleTime = model.audioContextTime + 0.1
                                    , processingState = model.currentState
                                }
                                    |> Song.play 0

                            else
                                Song.stop song
                    in
                    ( { model | isPlaying = isPlaying, song = Just newSong }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        TogglePiano ->
            ( { model | isPianoShown = not model.isPianoShown }, Cmd.none )

        ToggleMute channel ->
            case model.song of
                Just song ->
                    let
                        mutedChannels =
                            Array.indexedMap
                                (\i state ->
                                    if i == channel then
                                        not state

                                    else
                                        state
                                )
                                song.mutedChannels

                        newSong =
                            { song | mutedChannels = mutedChannels }
                    in
                    ( { model | song = Just newSong }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SetRowPosition delta ->
            -- base position on song.trackerStates
            case model.song of
                Just song ->
                    case Song.pickStateAtTime model.audioContextTime song of
                        Just stateWithTime ->
                            let
                                currentState =
                                    stateWithTime |> Tuple.second

                                newTrackerStates =
                                    LE.remove stateWithTime song.trackerStates

                                newSong =
                                    { song | trackerStates = newTrackerStates }
                            in
                            case model.currentState of
                                Song.State currentSongPosition currentPatternPosition ->
                                    ( { model
                                        | currentState = currentState
                                        , song = Just newSong
                                      }
                                    , setViewportOf "patterns" 0 (30 * toFloat (currentPatternPosition + 1)) |> Task.attempt (\_ -> NoOp)
                                    )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        PlayTick _ ->
            case model.song of
                Just song ->
                    case song.processingState of
                        Song.State processingSongPosition _ ->
                            if processingSongPosition >= song.mod.length then
                                ( { model
                                    | isPlaying = False
                                    , currentState = Song.State 0 0
                                    , song = Just (Song.stop song)
                                  }
                                , setRowPosition <| Song.State 0 0
                                )

                            else
                                ( { model | song = Just (Song.play 0 song) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        LoadModule url ->
            ( { model | currentState = Song.State 0 0 }, Cmd.batch [ setRowPosition <| Song.State 0 0, getModule url ] )


viewNote : Note -> Html Msg
viewNote note =
    let
        noteString =
            case note.period of
                Just period ->
                    Dict.get period Noteperiod.periodTable
                        |> Maybe.map (.name >> NoteName.toString)
                        |> Maybe.withDefault "---"

                Nothing ->
                    "---"

        instrumentString =
            Instrument.toString note.instrumentId
    in
    div [ class "pattern-table" ]
        [ span [ class "note" ] [ text noteString ]
        , span [ class "instrument" ] [ text instrumentString ]
        , span [ class "command" ] [ text (Effect.format note.effect) ]
        ]


viewPattern : Song -> Song.State -> Html Msg
viewPattern song currentState =
    case currentState of
        Song.State currentSongPosition currentPatternPosition ->
            let
                pattern =
                    Protracker.getRealPatternByIndex currentSongPosition song.mod |> Maybe.withDefault []

                viewMuteCell channel state =
                    th []
                        [ input [ type_ "checkbox", checked (not state), onClick <| ToggleMute channel ] [] ]

                channels =
                    Song.channelsNumber song.mod

                emptyRows =
                    List.range 0 4
                        |> List.map
                            (\_ ->
                                tr [ class "empty-row" ]
                                    (td [ class "position" ] [] :: (List.range 1 channels |> List.map (\_ -> td [] [])))
                            )
            in
            table [ class "patterns" ]
                [ thead []
                    [ tr [] ([ th [ class "position" ] [ text "Pos" ] ] ++ (song.mutedChannels |> Array.toList |> List.indexedMap viewMuteCell))
                    ]
                , tbody [ id "patterns" ]
                    (emptyRows
                        ++ List.indexedMap
                            (\i row ->
                                tr [ id <| "row-" ++ String.fromInt i, classList [ ( "active", i == currentPatternPosition ) ] ]
                                    (td [ class "position" ]
                                        [ span [ classList [ ( "highlighted", modBy 4 i == 0 ) ] ]
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


viewInstruments : List Instrument -> List Sample -> Html Msg
viewInstruments instruments samples =
    select [ size 10, id "instruments" ]
        (List.map2
            (\instrument sample ->
                let
                    id =
                        case instrument.id of
                            InstrumentId i ->
                                i

                    name =
                        [ text ((String.fromInt id |> String.padLeft 2 '0') ++ ":" ++ instrument.name) ]
                in
                if List.length sample.data > 0 then
                    option [ onClick (SelectInstrument <| Just instrument) ] name

                else
                    option [ onClick (SelectInstrument Nothing) ] name
            )
            instruments
            samples
        )


viewModule : Int -> Int -> PatternTable -> Html Msg
viewModule length currentSongPosition patternTable =
    select [ size 10, id "pattern-table" ]
        (List.range 0 (length - 1)
            |> List.map
                (\i ->
                    option [ onClick <| SelectPattern i, name <| "pattern-" ++ String.fromInt i, selected (i == currentSongPosition) ]
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


instrumentPianoNodes : Model -> WebAudio.Graph
instrumentPianoNodes model =
    case model.song of
        Nothing ->
            []

        Just song ->
            case Maybe.map2 Tuple.pair model.currentPianoInstrument model.currentPianoPeriod of
                Just ( instrument, period ) ->
                    [ Song.biquadFilterNode
                    , { id = WebAudio.NodeId "pianoGain"
                      , output = [ WebAudio.Output (WebAudio.NodeId "biquad") ]
                      , props = Song.instrumentGainNode instrument []
                      }
                    , { id = WebAudio.NodeId "piano"
                      , output = [ WebAudio.Output (WebAudio.NodeId "pianoGain") ]
                      , props = Song.instrumentBufferNode instrument period model.playPianoNow []
                      }
                    ]

                Nothing ->
                    []


samplesData : List Sample -> List ( WebAudio.Url, List Float )
samplesData samples =
    samples
        |> List.filterMap
            (\{ data, id } ->
                case id of
                    InstrumentId instrumentId ->
                        if List.length data > 0 then
                            Just ( WebAudio.Url (instrumentId |> String.fromInt), data )

                        else
                            Nothing
            )


viewAudio : Model -> Html Msg
viewAudio model =
    case model.song of
        Nothing ->
            text ""

        Just song ->
            let
                samplesList =
                    if model.samplesLoaded then
                        []

                    else
                        samplesData song.mod.samples
            in
            Keyed.node "div"
                []
                [ ( song.mod.title
                  , WebAudio.toHtml
                        { graph = instrumentPianoNodes model ++ Song.audioGraph song
                        , assets = []
                        , onProgress = AssetLoaded
                        , onTick = AudioContextTick
                        , samples = samplesList
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


viewPiano : Html Msg
viewPiano =
    div [ id "piano" ]
        [ ul [ class "set" ]
            (List.map viewPianoKey Noteperiod.list)
        ]


viewSongs : Bool -> Html Msg
viewSongs isPlaying =
    let
        click path =
            if isPlaying then
                []

            else
                [ onClick <| LoadModule path ]
    in
    select [ size 10, id "songs", disabled isPlaying ]
        [ option (click "/mods/a_better_love.mod") [ text "a_better_love" ]
        , option (click "/mods/ITSMYLIF.MOD") [ text "ITSMYLIFE" ]
        , option (click "/mods/kallisti-syndrome.mod") [ text "kallisti-syndrome" ]
        , option (click "/mods/lasttrip.mod") [ text "lasttrip" ]
        , option (click "/mods/necros-the_traveler.mod") [ text "necros-the_traveler" ]
        , option (click "/mods/only_a_song.mod") [ text "only_a_song" ]
        ]


view : Model -> Html Msg
view model =
    case model.error of
        Just err ->
            div []
                [ h2 [] [ text err ]
                , button [ onClick ModuleRequested ] [ text "Select" ]
                ]

        Nothing ->
            case model.song of
                Nothing ->
                    h2 [] [ text "loading" ]

                Just song ->
                    let
                        piano =
                            if model.isPianoShown then
                                div [] [ viewPiano ]

                            else
                                text ""
                    in
                    case model.currentState of
                        Song.State currentSongPosition _ ->
                            div []
                                [ h2 [] [ text song.mod.title ]
                                , h3 [] [ text <| "Song length: " ++ String.fromInt song.mod.length ]
                                , lazy viewAudio model
                                , viewSongs model.isPlaying
                                , lazy3 viewModule song.mod.length currentSongPosition song.mod.patternTable
                                , button [ onClick ModuleRequested, disabled model.isPlaying ] [ text "Select" ]
                                , button [ onClick TogglePlay ] [ text "Toggle Play" ]
                                , button [ onClick TogglePiano ] [ text "Toggle Piano" ]
                                , viewInstruments song.mod.instruments song.mod.samples
                                , br [] []
                                , br [] []
                                , button [ onClick FirstPattern ] [ text "<<" ]
                                , button [ onClick PrevPattern, disabled <| currentSongPosition == 0 ] [ text "<" ]
                                , button [ onClick NextPattern, disabled <| currentSongPosition == (song.mod.length - 1) ] [ text ">" ]
                                , button [ onClick LastPattern ] [ text ">>" ]
                                , lazy2 viewPattern song model.currentState
                                , br [] []
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
    if model.isPlaying then
        Sub.batch [ Time.every 1000 PlayTick, Browser.Events.onAnimationFrame SetRowPosition ]

    else
        Sub.none



-- HTTP


getModule : String -> Cmd Msg
getModule url =
    Http.get
        { url = url
        , expect = Http.expectBytes GotData Protracker.decoder
        }
