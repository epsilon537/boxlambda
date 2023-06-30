#!/usr/bin/env python3

#This python script analyzes the PCM data (input to the DAC) and 1-bit ouput data produced by 
#the audio_dac_test Verilator model.
#If, after passing through a low-pass filter, both correlate well, we have a pass.
#Results can also be plotted.

import sys
from dac_out import *
from pcm_out import *
import matplotlib.pyplot as plt
import numpy as np
import scipy
import getopt

CORR_THRESHOLD = 0.99

def dac_test(plot):

    #3rd order low pass butterworth filter
    #Cut-off at 25kHz, sampling frequency 12.5MHz
    sos = scipy.signal.butter(N=3, Wn=25000, btype='lowpass', fs=12500000, output='sos')

    y_pcm = np.array(pcmdata)
    y_norm_pcm = y_pcm/32768
    filtered_pcm = scipy.signal.sosfilt(sos, y_norm_pcm)
    normalized_filtered_pcm = filtered_pcm / np.linalg.norm(filtered_pcm)
    mags_pcm = np.abs(np.fft.fft(normalized_filtered_pcm))
    mags_pcm_db = 20*np.log10(mags_pcm)
    normalized_mags_pcm = mags_pcm / np.linalg.norm(mags_pcm)

    y_dac = np.array(dacdata)
    y_norm_dac = y_dac - 0.5
    filtered_dac = scipy.signal.sosfilt(sos, y_norm_dac)
    normalized_filtered_dac = filtered_dac / np.linalg.norm(filtered_dac)
    mags_dac = np.abs(np.fft.fft(normalized_filtered_dac))
    mags_dac_db = 20*np.log10(mags_dac)
    normalized_mags_dac = mags_dac / np.linalg.norm(mags_dac)

    time_dom_corr = np.correlate(a=normalized_filtered_pcm, v=normalized_filtered_dac)[0]
    freq_dom_corr = np.correlate(a=normalized_mags_pcm, v=normalized_mags_dac)[0]

    print("Time Domain Correlation: %f"%time_dom_corr)
    print("Frequency Domain Correlation: %f"%freq_dom_corr)

    if plot:
        plt.figure(1)
        plt.plot(normalized_filtered_dac[0:30000])
        plt.plot(normalized_filtered_pcm[0:30000])
        plt.figure(2)
        plt.plot(mags_dac_db[0:15000])
        plt.plot(mags_pcm_db[0:15000])
        plt.show()

    return (time_dom_corr > CORR_THRESHOLD) and (freq_dom_corr > CORR_THRESHOLD)

if __name__ == "__main__":
    try:
        opts, args = getopt.getopt(sys.argv[1:], "hp", ["help", "plot"])
    except getopt.GetoptError as err:
        # print help information and exit:
        print("Usage: dac_test.py [-h(elp)] [-p(lot)]")
        sys.exit(2)

    plot = False

    for o, a in opts:
        if o == "-v":
            verbose = True
        elif o in ("-h", "--help"):
            print("Usage: dac_test.py [-h(elp)] [-p(lot)]")
            sys.exit()
        elif o in ("-p", "--plot"):
            plot = True
        else:
            assert False, "unhandled option"

    if dac_test(plot=plot):
        sys.exit(0)
    else:
        sys.exit(-1)
