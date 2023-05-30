from dac_out import *
from pcm_out import *
import matplotlib.pyplot as plt
import numpy as np
import scipy

sos = scipy.signal.butter(N=3, Wn=25000, btype='lowpass', fs=12500000, output='sos')

y_pcm = np.array(pcmdata)
y_norm_pcm = y_pcm/32768
filtered_pcm = scipy.signal.sosfilt(sos, y_norm_pcm)
filtered_pcm_norm = np.linalg.norm(filtered_pcm)
filtered_pcm /= filtered_pcm_norm
mags_pcm = np.abs(np.fft.fft(filtered_pcm))
mags_pcm_db = 20*np.log10(mags_pcm)
#freqs = np.fft.fftfreq(len(y_norm),d=1/12500000.0)
#print("Freqs:")
#print(freqs[0:10])

y_dac = np.array(dacdata)
y_norm_dac = y_dac - 0.5
filtered_dac = scipy.signal.sosfilt(sos, y_norm_dac)
filtered_dac_norm = np.linalg.norm(filtered_dac)
filtered_dac /= filtered_dac_norm
mags_dac = np.abs(np.fft.fft(filtered_dac))
mags_dac_db = 20*np.log10(mags_dac)

print("Correlation: ")
print(np.correlate(a=filtered_pcm, v=filtered_dac)[0])

plt.figure(1)
plt.plot(filtered_dac[0:15000])
plt.plot(filtered_pcm[0:15000])
plt.figure(2)
plt.plot(mags_dac_db[0:15000])
plt.plot(mags_pcm_db[0:15000])
plt.show()

