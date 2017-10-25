import numpy as np
import matplotlib as mpl
import pylab as plt
import sys
from os import listdir
from os.path import isfile, join
mpl.rcParams['svg.fonttype'] = "none"
mpl.rcParams['font.size'] = 16
from matplotlib import pyplot as plt


e_val = np.loadtxt("eval.csv", delimiter=",")
e_out = np.loadtxt("eout.csv", delimiter=",")

linspace = np.linspace(-1, 1, len(e_val))

fig = plt.figure()
ax = fig.add_subplot(1,1,1)

ax.plot(linspace, e_out, label="\$E_{out}\$", color="#dd1c77")
ax.plot(linspace, e_val, label="\$E_{val}\$", color="#e34a33")



ax.spines["right"].set_visible(False)
ax.spines["top"].set_visible(False)
ax.xaxis.set_ticks_position("bottom")
ax.yaxis.set_ticks_position("left")
ax.set_xlabel("\$Size of validation set$")
ax.set_ylabel("\$Expected error\$")
ax.legend(loc="best", frameon=False, labelspacing=0.05)
plt.show()
