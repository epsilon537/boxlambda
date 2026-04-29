#!/usr/bin/env python3

from dac_out import *
import matplotlib.pyplot as plt
import numpy as np
import sys
import getopt
import scipy

# The test program running on the system generates the following 6 pitches.
REF_HZ = [440.00, 493.88, 523.55, 587.33, 659.25, 698.46]

def pitch_test(plot):
    #3rd order low pass butterworth filter
    #Cut-off at 25kHz, sampling frequency 12.5MHz
    sos = scipy.signal.butter(N=3, Wn=25000, btype='lowpass', fs=12500000, output='sos')

    #Raw normalized DAC output
    y_norm_dac_raw = np.array(dacdata) - 0.5
    y_filtered = scipy.signal.sosfilt(sos, y_norm_dac_raw)

    mags = np.abs(np.fft.fft(y_filtered))
    #d is the sample spacing, i.e. the inverse of the sampling rate, 12.5MHz.
    freqs = np.fft.fftfreq(len(y_filtered),d=1/12500000.0)

    #Analyze the frequency bins up to 1000Hz.
    bin_width_hz = freqs[1] - freqs[0]
    top_bin = int(1000/bin_width_hz)
    peak_bins = np.argsort(mags[0:top_bin])[::-1][0:6]
    peak_freqs = np.sort(freqs[peak_bins])
    deviations = (peak_freqs - REF_HZ)/REF_HZ

    print("Detected pitches: ")
    print(peak_freqs)

    print("Expected pitches:")
    print(REF_HZ)

    print("Relative Deviations:")
    print(deviations)

    if (plot):
        plt.plot(mags[0:top_bin])
        plt.show()

    if (np.max(np.abs(deviations)) < 0.2):
        print("Test Passed.")
        return 0
    else:
        print("Test Failed.")
        return -1

if __name__ == "__main__":
    try:
        opts, args = getopt.getopt(sys.argv[1:], "hp", ["help", "plot"])
    except getopt.GetoptError as err:
        # print help information and exit:
        print("Usage: ym2149_test.py [-h(elp)] [-p(lot)]")
        sys.exit(2)

    plot = False

    for o, a in opts:
        if o == "-v":
            verbose = True
        elif o in ("-h", "--help"):
            print("Usage: ym2149_test.py [-h(elp)] [-p(lot)]")
            sys.exit()
        elif o in ("-p", "--plot"):
            plot = True
        else:
            assert False, "unhandled option"

    res = pitch_test(plot=plot)
    sys.exit(res)