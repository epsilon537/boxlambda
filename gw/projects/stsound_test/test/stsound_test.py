#!/usr/bin/env python3

from pcm_out import *
import numpy as np
import scipy.io.wavfile
import sounddevice as sd
import correlation
import sys
import getopt

def stsound_test(ref_wav):
    y = np.array(pcmdata)
    y_ac = y - 32768
    y_norm = y_ac / 32768.0
    sr = int(50000000/1024)
    sd.play(y_norm, sr)
    sd.wait()
    scaled = np.int16(y_norm / np.max(np.abs(y_norm)) * 32767)
    scipy.io.wavfile.write('test.wav', sr, scaled)
    cor = correlation.correlate('test.wav',ref_wav)
    return cor > 0.8

if __name__ == "__main__":
    try:
        opts, args = getopt.getopt(sys.argv[1:], "hr:", ["help", "ref="])
    except getopt.GetoptError as err:
        # print help information and exit:
        print("Usage: stsound_test.py [-h(elp)] -r(ef) <reference wav file>")
        sys.exit(2)

    ref_wav = None

    for o, a in opts:
        if o in ("-h", "--help"):
            print("Usage: stsound_test.py [-h(elp)] -r(ef) <reference wav file>")
            sys.exit()
        if o in ("-r", "--ref"):
            ref_wav = a
        else:
            assert False, "unhandled option"

    if not ref_wav:
        print("Reference .wav file path must be passed in.")
        print("Usage: stsound_test.py [-h(elp)] -r(ef) <reference wav file>")
        sys.exit(-1)

    if stsound_test(ref_wav):
        res = 0
    else:
        res = 1
    
    sys.exit(res)
