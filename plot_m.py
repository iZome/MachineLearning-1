import numpy as np
import matplotlib as mpl
import pylab as plt
import sys
from os import listdir
from os.path import isfile, join
mpl.rcParams['svg.fonttype'] = "none"
mpl.rcParams['font.size'] = 16
from matplotlib import pyplot as plt

path = sys.argv[1]

files = [f for f in listdir(path) if isfile(join(path, f)) and "matrix" in f]

fig, ax = plt.subplots()

m = np.loadtxt("res.csv", delimiter=",")
norm = mpl.colors.Normalize(vmin=-0.2,vmax=1)

im = ax.imshow( m, cmap="Spectral", norm=norm,
extent=[0.2,1.1, m.shape[0]+20, 20], aspect='auto', \
#vmax=np.log(m.max()), vmin=-m.min()
)
plt.colorbar(im)
ax.set_xlabel("\$\sigma$")
ax.set_ylabel("\$N\$")
ax.spines["right"].set_visible(False)
ax.spines["top"].set_visible(False)
ax.spines["left"].set_visible(False)
ax.spines["bottom"].set_visible(False)

ax.xaxis.set_ticks_position('none')
ax.yaxis.set_ticks_position('none')

plt.show()
