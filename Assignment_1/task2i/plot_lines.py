import numpy as np
import matplotlib as mpl
import pylab as plt
import sys
from os import listdir
from os.path import isfile, join
mpl.rcParams['svg.fonttype'] = "none"
mpl.rcParams['font.size'] = 16
from matplotlib import pyplot as plt

l0 = np.loadtxt("tables/second-row90-col9-del_N1-del_sig0.100000.csv", delimiter=",")
l1 = np.loadtxt("tables/tenth-row90-col9-del_N1-del_sig0.100000.csv", delimiter=",")
l2 = np.loadtxt("tables/x-row90-col9-del_N1-del_sig0.100000.csv", delimiter=",")
l3 = np.loadtxt("tables/target-row90-col1-del_N1-del_sig0.100000.csv", delimiter=",")
l4 = np.loadtxt("tables/target_x_y-row90-col1-del_N1-del_sig0.100000.csv", delimiter=",")

fig = plt.figure()
ax = fig.add_subplot(1,1,1)

ax.plot(l2, l0, label="\$L_{1}\$")
ax.plot(l2, l1, label="\$L_{2}\$")
#ax.plot(l4, l3, label="\$L_{3}\$")



ax.spines["right"].set_visible(False)
ax.spines["top"].set_visible(False)
ax.xaxis.set_ticks_position("bottom")
ax.yaxis.set_ticks_position("left")
ax.set_xlabel("\$x$")
ax.set_ylabel("\$L_{q}\$")
ax.legend(loc="upper right", frameon=False, labelspacing=0.05)
plt.show()
