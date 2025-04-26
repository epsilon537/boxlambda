---
hide:
  - toc
---

## YM Music files and ST-Sound

- **ST-Sound Repo**, BoxLambda fork, `boxlambda` branch:
    [https://github.com/epsilon537/StSound](https://github.com/epsilon537/StSound).

- **ST-Sound Submodule in the BoxLambda Directory Tree**:
    boxlambda/sub/StSound/.

- **ST-Sound Component in the BoxLambda Directory Tree**:
    [boxlambda/sw/components/stsound](https://github.com/epsilon537/boxlambda/tree/master/sw/components/stsound)

The YM file format by Arnaud Carré, a.k.a. Leonard/Oxygene, is a music file format that supports the YM2149:

[http://leonard.oxg.free.fr/ymformat.html](http://leonard.oxg.free.fr/ymformat.html)

In addition to defining the file format, Arnaud Carré also provides **ST-Sound**, a library to play YM files:

[https://github.com/arnaud-carre/StSound](https://github.com/arnaud-carre/StSound)

The *ST-Sound* code base is written for Windows, but it was easy to add a Linux port using [PortAudio](http://www.portaudio.com/). I added the Linux port to my fork of the *StSound* repo:

[https://github.com/epsilon537/StSound](https://github.com/epsilon537/StSound)

I ported the ST-Sound library to BoxLambda, using the YM2149 audio core:

[https://github.com/epsilon537/boxlambda/tree/master/sw/components/stsound](https://github.com/epsilon537/boxlambda/tree/master/sw/components/stsound)

As you can see in the `CMakeLists.txt`, the port references some of the original *ST-Sound* library files unmodified in the *stsound* git submodule. The *ST-Sound* library files that required significant modification, I copied locally to the `boxlambda/sw/component/stsound/` directory.

The port is not complete. I did the bare minimum needed to be able to play back a simple YM-type 2 song. To play the more advanced subtypes, I need to add *digidrums* and *syncbuzzer* sound effects. I made a note of it.

### The ST-Sound Test Project

The ST-Sound test software build uses the BoxLambda *stsound* and *fatfs* libraries to load and play a YM file from an attached SD card. The Arty's switches and buttons can be used to control master volume, treble, and bass.

#### Audio Fingerprinting

The Verilator test bench of the ST-Sound test project records about 5s worth of generated samples and saves them off to `pcm_out.py`, similar to the previous two test builds. This brings up an interesting question, however: How do we analyze this data? I.e., how do we create an automated test that checks if the given PCM sample sequence sounds like the first 5s of the song we want to play back? A bit-for-bit check against a reference waveform would be too brittle. A slight delay, change in volume, treble, or bass would be sufficient to break the test. We need a test that checks if a given audio fragment *sounds like* another given audio fragment without requiring it to be 100% identical.

One technique that can be used for this purpose is called **Audio Fingerprinting**. The software package that implements it is called [Chromaprint](https://acoustid.org/chromaprint). It's a very interesting technique. Here is a nice article describing how it works:

[https://oxygene.sk/2011/01/how-does-chromaprint-work/](https://oxygene.sk/2011/01/how-does-chromaprint-work/)

For my test case, I'm borrowing `correlation.py`, a Python script that wraps around Chromaprint's *fpcalc* module:

[https://github.com/kdave/audio-compare](https://github.com/kdave/audio-compare)

The following article describes how the Python script works:

[https://shivama205.medium.com/audio-signals-comparison-23e431ed2207](https://shivama205.medium.com/audio-signals-comparison-23e431ed2207)

The script returns a correlation score. If we have an 80% match or more, the test passes.

The Verilator test bench is located here:

[https://github.com/epsilon537/boxlambda/blob/master/gw/projects/stsound_test/sim/sim_main.cpp](https://github.com/epsilon537/boxlambda/blob/master/gw/projects/stsound_test/sim/sim_main.cpp)

The test scripts and related files are here:

[https://github.com/epsilon537/boxlambda/tree/master/gw/projects/stsound_test/test](https://github.com/epsilon537/boxlambda/tree/master/gw/projects/stsound_test/test)

See [here](test-build-ym2149.md#st-sound-test-on-verilator) for instructions to build and run the test yourself.
