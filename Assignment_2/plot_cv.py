import numpy as np
import matplotlib as mpl
import pylab as plt
import sys
from os import listdir
from os.path import isfile, join
mpl.rcParams['svg.fonttype'] = "none"
mpl.rcParams['font.size'] = 16
from matplotlib import pyplot as plt


err = np.loadtxt("data/cv_error.csv", delimiter=",")

linspace = np.linspace(0.1, 10, 100)

fig = plt.figure()
ax = fig.add_subplot(1,1,1)

ax.plot(linspace, err, color="black")

ax.spines["right"].set_visible(False)
ax.spines["top"].set_visible(False)
ax.xaxis.set_ticks_position("bottom")
ax.yaxis.set_ticks_position("left")
ax.set_xlabel("\$\lambda$")
ax.set_ylabel("\$CV_{error}\$")
ax.legend(loc="best", frameon=False, labelspacing=0.05)
plt.show()
