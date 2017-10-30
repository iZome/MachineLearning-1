import numpy as np
import matplotlib as mpl
import pylab as plt
import sys
from os import listdir
from os.path import isfile, join
mpl.rcParams['svg.fonttype'] = "none"
mpl.rcParams['font.size'] = 16
from matplotlib import pyplot as plt

x = np.loadtxt("data/x.csv", delimiter=",")
y = np.loadtxt("data/y.csv", delimiter=",")
sin = np.loadtxt("data/sin.csv", delimiter=",")
best = np.loadtxt("data/best_model.csv", delimiter=",")

fig = plt.figure()
ax = fig.add_subplot(1,1,1)

ax.plot(x, best, color="#f03b20", label="\$\lambda = 2\.7\$")
ax.plot(x, sin, color="#feb24c", label="\$y = \\text{sin}(\pi \cdot x)\$")
ax.scatter(x, y, label="\$y = \\text{sin}(\pi \cdot x) + \epsilon(x)$", color="#a6d96a")

#ffeda0
#feb24c
#f03b20

ax.spines["right"].set_visible(False)
ax.spines["top"].set_visible(False)
ax.xaxis.set_ticks_position("bottom")
ax.yaxis.set_ticks_position("left")
ax.set_xlabel("\$x$")
ax.set_ylabel("\$y$")
ax.legend(loc="lower left", frameon=False, labelspacing=0.05)
plt.xlim(-1,1)
plt.show()
