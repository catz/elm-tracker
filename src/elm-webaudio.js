import createVirtualAudioGraph from 'virtual-audio-graph';
import {
    analyser,
    bufferSource,
    biquadFilter,
    channelMerger,
    channelSplitter,
    convolver,
    delay,
    dynamicsCompressor,
    gain,
    mediaElementSource,
    mediaStreamDestination,
    mediaStreamSource,
    oscillator,
    panner,
    stereoPanner,
    waveShaper
} from 'virtual-audio-graph';

customElements.define(
    "elm-webaudio",
    class extends HTMLElement {
        constructor() {
            super();
            this.virtualAudioGraph = null;
            this.audioGraphJson = [];
            this.audioBufferMap = new Map();
            this.arrayBufferMap = new Map();
            this.timerEnabled = true;
            this.wait = 20;
            this.samplesLoaded = false;

            const go = () => {
                if (this.virtualAudioGraph) {
                    const event = new CustomEvent("tick", { detail: this.virtualAudioGraph.currentTime });
                    this.dispatchEvent(event);
                }
                if (this.timerEnabled) {
                    setTimeout(go, this.wait);
                }
            }
            go();
            this.prepareAudioGraph();
            this.decodeBuffers();
        }

        prepareAudioGraph() {
            if (!this.virtualAudioGraph) {
                try {
                    this.virtualAudioGraph = createVirtualAudioGraph();
                    this.decodeBuffers()
                } catch {
                    // ignore
                }
            }
        }

        set samples(samples) {
            const audioContext = this.virtualAudioGraph.audioContext;

            if(this.samplesLoaded) {
                this.progress();
                return;
            }

            if(Array.isArray(samples)) {
                for (var i = 0; i < samples.length; i++) {
                    let sample = samples[i];
                    let sampleBuffer = audioContext.createBuffer(1, sample.data.length, audioContext.sampleRate);
                    let buffering = sampleBuffer.getChannelData(0);
                    for(var j=0; j < sample.data.length; j++) {
                      buffering[j] = sample.data[j];
                    }
                    this.audioBufferMap.set(sample.url, sampleBuffer);
                }
            }

            this.samplesLoaded = true;
        }

        set graph(value) {
            if(!this.samplesLoaded) {
                return;
            }

            this.prepareAudioGraph();
            this.decodeBuffers();
            this.audioGraphJson = value;
            if (this.virtualAudioGraph) {
                this.virtualAudioGraph.update(this.jsonToVirtualWebAudioGraph(value));
            }
        }

        set assets(value) {
            this.prepareAudioGraph();
            this.decodeBuffers();
            if (this.virtualAudioGraph) {
                for (let url of value) {
                    this.getAudioBuffer(url);
                }
            }
        }

        connectedCallback() {
            this.prepareAudioGraph();
            this.decodeBuffers();
            if (this.virtualAudioGraph) {
                this.virtualAudioGraph.update({});
            }
        }

        disconnectedCallback() {
            this.timerEnabled = false;
        }

        jsonToVirtualWebAudioGraph(nodes) {
            const vgraph = {};
            Object.keys(nodes).forEach(key => {
                const props = nodes[key];
                switch (props.node) {
                    case "Analyser":
                        vgraph[key] = analyser(props.output, {
                            fftSize: props.fftSize,
                            minDecibels: props.minDecibels,
                            maxDecibels: props.maxDecibels,
                            smoothingTimeConstant: props.smoothingTimeConstant
                        });
                        break;
                    case "BufferSource":
                        const audioBuffer = this.getAudioBuffer(props.buffer);
                        if (audioBuffer) {
                            vgraph[key] = bufferSource(props.output, {
                                buffer: audioBuffer,
                                startTime: props.startTime,
                                stopTime: props.stopTime,
                                // detune: props.detune, // doesn't work in safari - https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode/detune
                                playbackRate: props.playbackRate,
                                loop: props.loop,
                                loopStart: props.loopStart,
                                loopEnd: props.loopEnd,
                                offsetTime: props.offsetTime
                            });
                        } else {
                            vgraph[key] = gain(props.output, {});
                        }
                        break;
                    case "BiquadFilter":
                        vgraph[key] = biquadFilter(props.output, {
                            type: props.type,
                            frequency: props.frequency,
                            detune: props.detune,
                            Q: props.Q
                        });
                        break;
                    case "ChannelMerger":
                        vgraph[key] = channelMerger(props.output, {});
                        break;
                    case "ChannelSplitter":
                        vgraph[key] = channelSplitter(props.output, {});
                        break;
                    case "Convolver":
                        // Workaround: `buffer` property of convolver can't be null.
                        const ir = this.getAudioBuffer(props.buffer);
                        if (ir) {
                            vgraph[key] = convolver(props.output, { buffer: ir, normalize: props.normalize });
                        } else {
                            vgraph[key] = gain(props.output);
                        }
                        break;
                    case "Delay":
                        vgraph[key] = delay(props.output, { delayTime: props.delayTime });
                        break;
                    case "DynamicsCompressor":
                        vgraph[key] = dynamicsCompressor(props.output, {
                            threshold: props.threshold,
                            knee: props.knee,
                            ratio: props.ratio,
                            attack: props.attack,
                            release: props.release
                        });
                        break;
                    case "Gain":
                        vgraph[key] = gain(props.output, { gain: props.gain });
                        break;
                    case "MediaElementSource":
                        vgraph[key] = mediaElementSource(props.output, { mediaElement: document.getElementById(props.mediaElement) });
                        break;
                    case "MediaStreamDestination":
                        vgraph[key] = mediaStreamDestination(props.output, {});
                        break;
                    case "MediaStreamSource":
                        vgraph[key] = mediaStreamSource(props.output, { mediaStream: props.mediaStream });
                        break;
                    case "Oscillator":
                        vgraph[key] = oscillator(props.output, { type: props.type, frequency: props.frequency, detune: 0, startTime: props.startTime, stopTime: props.stopTime });
                        break;
                    case "Panner":
                        vgraph[key] = panner(props.output, {
                            coneInnerAngle: props.coneInnerAngle,
                            coneOuterAngle: props.coneOuterAngle,
                            coneOuterGain: props.coneOuterGain,
                            distanceModel: props.distanceModel,
                            orientation: [props.orientatonX, props.orientationY, props.orientationZ],
                            panningModel: props.pannerModel,
                            position: [props.positionX, props.positionY, props.positionZ],
                            maxDistance: props.maxDistance,
                            refDistance: props.refDistance,
                            rolloffFactor: props.rolloffFactor,
                        });
                        break;
                    case "StereoPanner":
                        const audioContext = this.virtualAudioGraph.audioContext;
                        if(audioContext.createStereoPanner) {
                          vgraph[key] = stereoPanner(props.output, { pan: props.pan })
                        } else {
                          vgraph[key] = gain(props.output, { gain: 1 });
                        }

                        break;
                    case "WaveShaper":
                        vgraph[key] = waveShaper(props.output, {
                            curve: Float32Array.from(props.curve),
                            oversample: props.oversample
                        })
                        break;
                    default:
                        debugger;
                        throw new Error("Unsupported audio node: " + json.node);
                }
            });
            return vgraph;
        }

        getAudioBuffer(url) {

            if (!url) {
                return null;
            }

            const buffer = this.audioBufferMap.get(url);

            if (buffer === "loading") {
                return null;
            } else if (buffer instanceof ArrayBuffer) {
                return null;
            } else if (buffer === "decoding") {
                return null;
            } else if (buffer instanceof AudioBuffer) {
                return buffer;
            } else if (buffer) {
                throw new Error();
            } else {
                this.audioBufferMap.set(url, "loading");
                fetch(url).then(response => {
                    return response.arrayBuffer().then(arrayBuffer => {
                        this.audioBufferMap.set(url, arrayBuffer);
                        this.decodeBuffers();
                    });
                }).catch(err => {
                    this.audioBufferMap.delete(url);
                    console.error("getAudioBuffer: " + err + ", url: " + url);
                });
                return null;

            }
        }

        decodeBuffers() {
            this.audioBufferMap.forEach((arrayBuffer, url) => {
                if (arrayBuffer === "loading") {
                    // ignore
                } else if (arrayBuffer instanceof ArrayBuffer) {
                    if (this.audioBufferMap) {
                        this.audioBufferMap.set(url, "decoding");
                        return this.virtualAudioGraph.audioContext.decodeAudioData(arrayBuffer).then(decoded => {
                            this.audioBufferMap.set(url, decoded);
                            this.virtualAudioGraph.update(this.jsonToVirtualWebAudioGraph(this.audioGraphJson));
                            this.progress();
                        }).catch(e => {
                            this.audioBufferMap.delete(url);
                            console.error("decodeBuffers: " + e + ", url: " + url);
                        });
                    } else {
                        // ignore
                    }
                } else if (arrayBuffer === "decoding") {
                    // ignore
                } else if (arrayBuffer instanceof AudioBuffer) {
                    // ignore
                } else {
                    throw new Error();
                }
            });
        }

        progress() {
            const states = [];
            this.audioBufferMap.forEach((value, url) => {
                if (value === "loading" || value == "decoding") {
                    // ignore
                } else if (value instanceof ArrayBuffer) {
                    // ignore
                } else if (value instanceof AudioBuffer) {
                    states.push(url);
                } else {
                    throw new Error();
                }
            });
            const event = new CustomEvent("progress", { detail: states });
            this.dispatchEvent(event);
        }
    }
);
