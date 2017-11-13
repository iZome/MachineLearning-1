import numpy as np
import matplotlib as mpl
import pylab as plt
import sys
from os import listdir
from os.path import isfile, join
mpl.rcParams['svg.fonttype'] = "none"
mpl.rcParams['font.size'] = 16
from matplotlib import pyplot as plt

m = np.loadtxt("averaged_matrix.csv", delimiter=",")
m = np.rot90(m, k=1)

fig = plt.figure()
ax = fig.add_subplot(1,1,1)

norm = mpl.colors.Normalize(vmin=-0.2, vmax=0.2)

im = ax.imshow(m, cmap="jet", norm=norm, interpolation='gaussian',
extent=[20, m.shape[1]+20, 0.1,2.0],
aspect='auto', \
#vmax=np.log(m.max()), vmin=-m.min()
)



plt.colorbar(im)
ax.set_ylabel("\$\sigma$")
ax.set_xlabel("\$N\$")
ax.spines["right"].set_visible(False)
ax.spines["top"].set_visible(False)
ax.spines["left"].set_visible(False)
ax.spines["bottom"].set_visible(False)

ax.xaxis.set_ticks_position('none')
ax.yaxis.set_ticks_position('none')

plt.show()
