import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import './elm-webaudio'

import createVirtualAudioGraph, {
  createNode,
  createWorkletNode,
  delay,
  gain,
  oscillator,
  stereoPanner,
  bufferSource,
} from 'virtual-audio-graph';


window.AudioContext = window.AudioContext || window.webkitAudioContext;
if (typeof window.AudioContext === 'undefined') {
  alert('Sorry, this browser does not support the Web Audio API.');
}

const app = Elm.Main.init({
  node: document.getElementById('root')
});

registerServiceWorker();

const audioContext = new AudioContext();
const virtualAudioGraph = createVirtualAudioGraph({
  audioContext: audioContext,
  output: audioContext.destination
});

const { currentTime } = virtualAudioGraph;



//app.ports.renderSample.subscribe(function(sample) {
//  var sampleBuffer = audioContext.createBuffer(1, sample.length, audioContext.sampleRate);
//
//    var buffering = sampleBuffer.getChannelData(0);
//    for(var i=0; i < sample.length; i++) {
//      buffering[i] = sample[i];
//    }
//
//    var source = audioContext.createBufferSource();
//    source.buffer = sampleBuffer;
//
//    console.log(sampleBuffer)
//
//    source.connect(audioContext.destination);
//    source.start(audioContext.currentTime, 0);
//});
