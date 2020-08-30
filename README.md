# Elm Tracker

#### Goals

* An attempt to create minimal working tracker app in [Elm](https://elm-lang.org/).
* Learn elm/bytes
* Work with Web Audio API

[Live demo](https://elm-tracker.netlify.com/)

## Tested on

* Desktop Mac Chrome
* Desktop Mac Firefox

Not tested on mobile devices.

## Local start

This project is bootstrapped with [Create Elm App](https://github.com/halfzebra/create-elm-app).

Due to "RangeError: Maximum call stack size exceeded" bug we need to turn off time-travel debugger.

```sh
ELM_DEBUGGER=false elm-app start
```

## Useful links

* [Webbased old-school Amiga music tracker in plain old javascript](https://github.com/steffest/bassoontracker)
* [MilkyTracker](https://milkytracker.titandemo.org/downloads/)
* [PROTRACKER FILE FORMAT](http://coppershade.org/articles/More!/Topics/Protracker_File_Format/)
* [A Tale of Two Clocks - Scheduling Web Audio with Precision](https://www.html5rocks.com/en/tutorials/audio/scheduling/)
* [elm-webaudio](https://github.com/aratama/elm-webaudio)
* TODO: For better scheduling [A comprehensive event scheduling tool for Web Audio API.](https://github.com/sebpiq/WAAClock)