import numpy as np
import matplotlib as mpl
import pylab as plt
import sys
from os import listdir
from os.path import isfile, join
mpl.rcParams['svg.fonttype'] = "none"
mpl.rcParams['font.size'] = 16
from matplotlib import pyplot as plt

linspace = np.linspace(-1, 1, 500)

l0 = np.loadtxt("0.csv", delimiter=",")
l1 = np.loadtxt("1.csv", delimiter=",")
l2 = np.loadtxt("2.csv", delimiter=",")
l3 = np.loadtxt("3.csv", delimiter=",")
l4 = np.loadtxt("4.csv", delimiter=",")
l5 = np.loadtxt("5.csv", delimiter=",")

fig = plt.figure()
ax = fig.add_subplot(1,1,1)
ax.plot(linspace, l0, label="\$L_{0}\$")
ax.plot(linspace, l1, label="\$L_{1}\$")
ax.plot(linspace, l2, label="\$L_{2}\$")
ax.plot(linspace, l3, label="\$L_{3}\$")
ax.plot(linspace, l4, label="\$L_{4}\$")
ax.plot(linspace, l5, label="\$L_{5}\$")


ax.spines["right"].set_visible(False)
ax.spines["top"].set_visible(False)
ax.xaxis.set_ticks_position("bottom")
ax.yaxis.set_ticks_position("left")
ax.set_xlabel("\$x$")
ax.set_ylabel("\$L_{q}\$")
ax.legend(loc="upper center", frameon=False, labelspacing=0.05)
plt.show()
