module Song exposing (Song, State(..), audioGraph, biquadFilterNode, channelsNumber, create, instrumentBufferNode, instrumentGainNode, pickStateAtTime, play, stop)

import Array exposing (Array)
import Bitwise
import Effect exposing (Effect(..))
import Instrument exposing (Instrument, InstrumentId(..))
import List.Extra
import Noteperiod exposing (getFineTuneForPeriod)
import Protracker exposing (Pattern, ProTracker, Row)
import WebAudio


type State
    = State Int Int


type alias Song =
    { mod : ProTracker
    , processingState : State
    , bpm : Int
    , ticksPerStep : Int
    , activeRows : List ( ( Float, Float, Int ), Row )
    , scheduleTime : Float
    , instruments : Array Instrument
    , trackerStates : List ( Float, State )
    , mutedChannels : Array Bool
    }


type alias EffectNodeData =
    { time : Float
    , effect : Effect
    , resetOnStep : Bool
    , tickTime : Float
    , ticksPerStep : Int
    }


type alias InstrumentNodeData =
    { channel : Int
    , period : Maybe Int
    , time : Float
    , cutOffTime : Maybe Float
    , effects : List EffectNodeData
    , instrument : Maybe Instrument
    }


create : ProTracker -> Song
create mod =
    { mod = mod
    , processingState = State 0 0
    , bpm = 125
    , ticksPerStep = 6
    , activeRows = []
    , scheduleTime = 0
    , instruments = mod.instruments |> Array.fromList
    , trackerStates = []
    , mutedChannels = Array.initialize (channelsNumber mod) (\_ -> False)
    }


channelsNumber : ProTracker -> Int
channelsNumber mod =
    mod.patterns
        |> List.head
        |> Maybe.andThen List.head
        |> Maybe.map List.length
        |> Maybe.withDefault 0


setAmigaSpeed : Int -> Song -> Song
setAmigaSpeed speed model =
    { model | ticksPerStep = speed }


setBPM : Int -> Song -> Song
setBPM newBPM model =
    { model | bpm = newBPM }


processSpeed : Row -> Song -> Song
processSpeed row model =
    List.foldl
        (\note newModel ->
            case note.effect of
                SetSpeed param ->
                    if param <= 32 then
                        setAmigaSpeed param newModel

                    else
                        setBPM param newModel

                _ ->
                    newModel
        )
        model
        row


pickStateAtTime : Float -> Song -> Maybe ( Float, State )
pickStateAtTime audioContextTime song =
    song.trackerStates
        |> List.Extra.find (\( time, _ ) -> time < audioContextTime)


selectPattern : Int -> Song -> Song
selectPattern pos song =
    case song.processingState of
        State processingSongPosition processingPatternPosition ->
            { song | processingState = State pos processingPatternPosition }


nextPattern : Song -> Song
nextPattern song =
    case song.processingState of
        State processingSongPosition processingPatternPosition ->
            { song | processingState = State (processingSongPosition + 1) processingPatternPosition }


prevPattern : Song -> Song
prevPattern song =
    case song.processingState of
        State processingSongPosition processingPatternPosition ->
            { song | processingState = State (processingSongPosition - 1) processingPatternPosition }


firstPattern : Song -> Song
firstPattern song =
    case song.processingState of
        State processingSongPosition processingPatternPosition ->
            { song | processingState = State 0 processingPatternPosition }


lastPattern : Song -> Song
lastPattern song =
    case song.processingState of
        State processingSongPosition processingPatternPosition ->
            { song | processingState = State (song.mod.length - 1) processingPatternPosition }


firstRow : Song -> Song
firstRow song =
    case song.processingState of
        State processingSongPosition processingPatternPosition ->
            { song | processingState = State processingSongPosition 0 }


nextRow : Song -> Song
nextRow song =
    case song.processingState of
        State processingSongPosition processingPatternPosition ->
            if processingPatternPosition >= 63 then
                song
                    |> firstRow
                    |> nextPattern

            else
                { song | processingState = State processingSongPosition (processingPatternPosition + 1) }


maxTime : Float
maxTime =
    1


stop : Song -> Song
stop song =
    { song
        | activeRows = []
        , scheduleTime = 0
        , trackerStates = []
    }
        |> firstRow
        |> firstPattern


play : Float -> Song -> Song
play time song =
    if time < maxTime then
        case song.processingState of
            State processingSongPosition processingPatternPosition ->
                if processingSongPosition >= song.mod.length then
                    song

                else
                    let
                        row =
                            Protracker.getRealPatternByIndex processingSongPosition song.mod
                                |> Maybe.andThen (Protracker.getRowByIndex processingPatternPosition)
                                |> Maybe.withDefault []

                        newSong =
                            song
                                |> processSpeed row
                                |> nextRow

                        tickTime =
                            2.5 / toFloat newSong.bpm

                        delta =
                            toFloat song.ticksPerStep * tickTime

                        newTime =
                            newSong.scheduleTime + delta
                    in
                    play (time + delta)
                        { newSong
                            | activeRows = ( ( newTime, tickTime, newSong.ticksPerStep ), row ) :: song.activeRows
                            , trackerStates = ( newTime, newSong.processingState ) :: newSong.trackerStates
                            , scheduleTime = newTime
                        }

    else
        -- TODO better cleanup for activeRows required
        { song | activeRows = List.take 128 song.activeRows }


instrumentGainNode : Instrument -> List EffectNodeData -> WebAudio.Props
instrumentGainNode instrument effects =
    let
        defaultVolume =
            100 * toFloat instrument.volume / 64

        gain =
            case effects of
                [] ->
                    WebAudio.Constant (defaultVolume / 100)

                _ ->
                    effects
                        |> List.reverse
                        |> List.foldl
                            (\effectData ( currentVolume, acc ) ->
                                case effectData.effect of
                                    VolumeSlide param ->
                                        let
                                            value =
                                                if param < 16 then
                                                    --  slide down
                                                    (toFloat param * -1) * 100 / 64

                                                else
                                                    -- slide up
                                                    toFloat (param |> Bitwise.shiftRightBy 4) * 100 / 64

                                            newCurrentVolume =
                                                if effectData.resetOnStep then
                                                    defaultVolume

                                                else
                                                    currentVolume

                                            ( newCurrentVolume2, volumes ) =
                                                List.range 1 (effectData.ticksPerStep - 1)
                                                    |> List.foldl
                                                        (\tick ( volume, prevVolumes ) ->
                                                            let
                                                                volumeNormalized =
                                                                    max volume 0
                                                                        |> min 100
                                                            in
                                                            ( volumeNormalized + value, WebAudio.SetValueAtTime (volumeNormalized / 100) (WebAudio.Time (effectData.time + (toFloat tick * effectData.tickTime))) :: prevVolumes )
                                                        )
                                                        ( newCurrentVolume, [] )
                                        in
                                        ( newCurrentVolume2, List.reverse volumes :: acc )

                                    SetVolume param ->
                                        let
                                            volume =
                                                toFloat param / 64 * 100
                                        in
                                        ( volume, [ WebAudio.SetValueAtTime (volume / 100) (WebAudio.Time effectData.time) ] :: acc )

                                    _ ->
                                        let
                                            volume =
                                                defaultVolume
                                        in
                                        ( volume, [ WebAudio.SetValueAtTime (defaultVolume / 100) (WebAudio.Time effectData.time) ] :: acc )
                            )
                            ( defaultVolume, [] )
                        |> Tuple.second
                        |> List.concat
                        |> WebAudio.Methods
    in
    WebAudio.Gain { gain = gain }


instrumentCutOffNode : Bool -> Maybe Float -> WebAudio.Props
instrumentCutOffNode muted cutOffTime =
    if muted then
        WebAudio.Gain
            { gain =
                WebAudio.Constant 0
            }

    else
        case cutOffTime of
            Just time ->
                WebAudio.Gain
                    { gain =
                        WebAudio.Methods
                            [ WebAudio.SetValueAtTime (toFloat 1) (WebAudio.Time (time - 0.002))
                            , WebAudio.LinearRampToValueAtTime (toFloat 0) (WebAudio.Time time)
                            ]
                    }

            Nothing ->
                WebAudio.Gain
                    { gain =
                        WebAudio.Constant 1
                    }


instrumentBufferNode : Instrument -> Int -> Float -> List EffectNodeData -> WebAudio.Props
instrumentBufferNode { id, loop, length, finetune } period time effects =
    let
        fineTunedPeriod =
            getFineTuneForPeriod period finetune

        sampleRate =
            Noteperiod.getSampleRateForPeriod fineTunedPeriod

        audioContextSampleRate =
            48000

        initialPlaybackRate =
            sampleRate / audioContextSampleRate

        (InstrumentId instrumentId) =
            id

        loopEnabled =
            if loop.enabled && loop.length > 2 then
                True

            else
                False

        loopStart =
            toFloat loop.start / audioContextSampleRate

        loopEnd =
            toFloat (loop.start + loop.length) / audioContextSampleRate

        offsetTime =
            case List.reverse effects of
                [ effectNodeData ] ->
                    case effectNodeData.effect of
                        SampleOffset param ->
                            let
                                value =
                                    param |> Bitwise.shiftLeftBy 8

                                value2 =
                                    if value > length then
                                        toFloat (length - 1) / audioContextSampleRate

                                    else
                                        toFloat value / audioContextSampleRate
                            in
                            Just (WebAudio.Time value2)

                        _ ->
                            Nothing

                _ ->
                    Nothing
    in
    WebAudio.BufferSource
        { buffer = WebAudio.Url (instrumentId |> String.fromInt)
        , detune = 0
        , startTime = WebAudio.Time time
        , stopTime = Nothing
        , playbackRate = initialPlaybackRate
        , loop = loopEnabled
        , loopStart = loopStart
        , loopEnd = loopEnd
        , offsetTime = offsetTime
        }


volumeGainNode : WebAudio.Node
volumeGainNode =
    { id = WebAudio.NodeId "volumeGain"
    , output = WebAudio.output
    , props =
        WebAudio.Gain
            { gain =
                WebAudio.Constant 0.5
            }
    }


biquadFilterNode : WebAudio.Node
biquadFilterNode =
    { id = WebAudio.NodeId "biquad"
    , output = [ WebAudio.Output (WebAudio.NodeId "volumeGain") ]
    , props =
        WebAudio.BiquadFilter
            { type_ = WebAudio.Lowpass
            , frequency =
                WebAudio.Methods
                    [ WebAudio.SetValueAtTime (toFloat 20000) (WebAudio.Time 0)
                    ]
            , detune = WebAudio.Constant 0
            , q = WebAudio.Constant 1
            }
    }


stereoSeparationNodes : List WebAudio.Node
stereoSeparationNodes =
    List.map
        (\value ->
            let
                name =
                    if value < 0 then
                        "Left"

                    else
                        "Right"
            in
            { id = WebAudio.NodeId ("stereoPanner" ++ name)
            , output = [ WebAudio.Output (WebAudio.NodeId "biquad") ]
            , props =
                WebAudio.StereoPanner
                    { pan = WebAudio.Constant value
                    }
            }
        )
        [ -0.5, 0.5 ]


instrumentNode : Array Bool -> InstrumentNodeData -> List WebAudio.Node
instrumentNode mutedChannels data =
    let
        channelMuted =
            Array.get data.channel mutedChannels |> Maybe.withDefault False
    in
    case Maybe.map2 Tuple.pair data.period data.instrument of
        Just ( period, instrument ) ->
            let
                id =
                    String.fromInt data.channel ++ String.fromFloat data.time

                stereoPannerNodeName =
                    if modBy 2 data.channel == 0 then
                        "stereoPannerLeft"

                    else
                        "stereoPannerRight"
            in
            [ { id = WebAudio.NodeId ("instrumentChannelGainNode" ++ id)
              , output = [ WebAudio.Output (WebAudio.NodeId stereoPannerNodeName) ]
              , props = instrumentCutOffNode channelMuted data.cutOffTime
              }
            , { id = WebAudio.NodeId ("instrumentGainNode" ++ id)
              , output = [ WebAudio.Output (WebAudio.NodeId ("instrumentChannelGainNode" ++ id)) ]
              , props = instrumentGainNode instrument data.effects
              }
            , { id = WebAudio.NodeId ("instrumentBufferNode" ++ id)
              , output = [ WebAudio.Output (WebAudio.NodeId ("instrumentGainNode" ++ id)) ]
              , props = instrumentBufferNode instrument period data.time data.effects
              }
            ]

        Nothing ->
            []


audioGraph : Song -> WebAudio.Graph
audioGraph song =
    let
        instrumentData =
            song.activeRows
                |> List.reverse
                |> List.foldl
                    (\( ( time, tickTime, ticksPerStep ), row ) acc ->
                        let
                            nodes =
                                row
                                    |> List.indexedMap
                                        (\channel note ->
                                            let
                                                reset =
                                                    Maybe.map (always True) note.instrumentId |> Maybe.withDefault False

                                                effects =
                                                    case note.effect of
                                                        None ->
                                                            []

                                                        effect ->
                                                            [ EffectNodeData time effect reset tickTime ticksPerStep ]
                                            in
                                            case note.instrumentId of
                                                Just (InstrumentId instrumentId) ->
                                                    Array.get (instrumentId - 1) song.instruments
                                                        |> Maybe.map (\instrument -> InstrumentNodeData channel note.period time Nothing effects (Just instrument))

                                                Nothing ->
                                                    case note.effect of
                                                        None ->
                                                            Nothing

                                                        _ ->
                                                            Just <| InstrumentNodeData channel note.period time Nothing effects Nothing
                                        )
                                    |> List.filterMap identity
                        in
                        nodes
                            |> List.foldl
                                (\data newAcc ->
                                    case Array.get data.channel newAcc of
                                        Just [] ->
                                            Array.set data.channel (data :: []) newAcc

                                        Just (oldInstrumentData :: channelData) ->
                                            let
                                                reset =
                                                    Maybe.map (always True) data.instrument |> Maybe.withDefault False

                                                effects =
                                                    data.effects ++ oldInstrumentData.effects
                                            in
                                            case data.period of
                                                Just _ ->
                                                    Array.set data.channel (data :: { oldInstrumentData | cutOffTime = Just data.time, effects = effects } :: channelData) newAcc

                                                Nothing ->
                                                    -- join effect to existing instrument node
                                                    Array.set data.channel ({ oldInstrumentData | effects = effects } :: channelData) newAcc

                                        _ ->
                                            newAcc
                                )
                                acc
                    )
                    (Array.initialize 32 (always []))

        instrumentNodes =
            instrumentData
                |> Array.toList
                |> List.concat
                |> List.map (instrumentNode song.mutedChannels)
                |> List.concat
    in
    volumeGainNode :: biquadFilterNode :: stereoSeparationNodes ++ instrumentNodes
