import numpy as np
import matplotlib as mpl
import pylab as plt
import sys
from os import listdir
from os.path import isfile, join
mpl.rcParams['svg.fonttype'] = "none"
mpl.rcParams['font.size'] = 16
from matplotlib import pyplot as plt

path = sys.argv[1] + "/data/"

colors = ["#d7191c", "#fdae61", "#a6d96a", "#1a9641"]



x = np.loadtxt("data/x.csv", delimiter=",")
y = np.loadtxt("data/y.csv", delimiter=",")
sin = np.loadtxt("data/sin.csv", delimiter=",")
#m1 = np.loadtxt("data/model_reg_0.csv", delimiter=",")
#m2 = np.loadtxt("data/model_reg_5.csv", delimiter=",")



fig = plt.figure()
ax = fig.add_subplot(1,1,1)

#ax.plot(x, m1, color=colors[0], label="\$\lambda = %d\$"%(0))
#ax.plot(x, m2, color=colors[1], label="\$\lambda = %d\$"%(5))
ax.scatter(x, y, color=colors[2], label="\$y = \\text{sin}(\pi \cdot x) + \epsilon(x)$")
ax.plot(x, sin, color=colors[1], label="\$y = \\text{sin}(\pi \cdot x)\$")







ax.spines["right"].set_visible(False)
ax.spines["top"].set_visible(False)
ax.xaxis.set_ticks_position("bottom")
ax.yaxis.set_ticks_position("left")
ax.set_xlabel("\$x$")
ax.set_ylabel("\$y\$")
ax.legend(loc="lower left", frameon=False, labelspacing=0.05)
plt.xlim(-1, 1)
plt.show()
