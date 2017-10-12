import numpy as np
import matplotlib as mpl
import pylab as plt
import sys
from os import listdir
from os.path import isfile, join
mpl.rcParams['svg.fonttype'] = "none"
mpl.rcParams['font.size'] = 16
from matplotlib import pyplot as plt

linspace = np.linspace(-1, 1, 201)

l0 = np.loadtxt("alpha_1.csv", delimiter=",")
l1 = np.loadtxt("alpha_2.csv", delimiter=",")
l2 = np.loadtxt("alpha_3.csv", delimiter=",")

fig = plt.figure()
ax = fig.add_subplot(1,1,1)

ax.plot(linspace, l0, label="\$L_{1}\$")
ax.plot(linspace, l1, label="\$L_{2}\$")
ax.plot(linspace, l2, label="\$L_{3}\$")



ax.spines["right"].set_visible(False)
ax.spines["top"].set_visible(False)
ax.xaxis.set_ticks_position("bottom")
ax.yaxis.set_ticks_position("left")
ax.set_xlabel("\$x$")
ax.set_ylabel("\$L_{q}\$")
ax.legend(loc="upper right", frameon=False, labelspacing=0.05)
plt.show()
