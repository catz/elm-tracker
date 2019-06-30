import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import './elm-webaudio'

import * as ElmDebugger from 'elm-debug-transformer';

ElmDebugger.register();

window.AudioContext = window.AudioContext || window.webkitAudioContext;
if (typeof window.AudioContext === 'undefined') {
  alert('Sorry, this browser does not support the Web Audio API.');
}

const app = Elm.Main.init({
  node: document.getElementById('root')
});

registerServiceWorker();