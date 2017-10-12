import numpy as np
import matplotlib as mpl
import sys
from os import listdir
from os.path import isfile, join
mpl.rcParams['svg.fonttype'] = "none"
mpl.rcParams['font.size'] = 16
from matplotlib import pyplot as plt


test = np.loadtxt("matrix.csv", delimiter=",")

'''
fig = plt.figure()
ax = fig.add_subplot(1,1,1)
ax.plot(linspace, sol, label="\$L_{1}\$")


ax.spines["right"].set_visible(False)
ax.spines["top"].set_visible(False)
ax.xaxis.set_ticks_position("bottom")
ax.yaxis.set_ticks_position("left")
ax.set_xlabel("\$x$")
ax.set_ylabel("\$L_{1}\$")
ax.legend(loc="right", frameon=False, labelspacing=0.05)
plt.show()
'''
