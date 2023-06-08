from pcmdata import *
import matplotlib.pyplot as plt
import numpy as np
import sys
import getopt

def pitch_test(plot):
    y = np.array(pcmdata)
    y_ac = y - 32768.0
    y_norm = y_ac / 32768.0
        
    ref_hz = [440.00, 493.88, 523.55, 587.33, 659.25, 698.46]

    mags = np.abs(np.fft.fft(y_norm))
    freqs = np.fft.fftfreq(len(y_norm),d=1/48800.0)
    bin_width_hz = freqs[1] - freqs[0]
    top_bin = int(1000/bin_width_hz)
    peak_bins = np.argsort(mags[0:top_bin])[::-1][0:6]
    peak_freqs = np.sort(freqs[peak_bins])
    deviations = (peak_freqs - ref_hz)/ref_hz


    print("Detected pitches: ")
    print(peak_freqs)

    print("Expected pitches:")
    print(ref_hz)


    print("Relative Deviations:")
    print(deviations)

    plt.magnitude_spectrum(y_norm, Fs=48800)

    if (plot):
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