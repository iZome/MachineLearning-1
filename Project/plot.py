import numpy as np
import matplotlib as mpl
import pylab as plt
import sys
from os import listdir
from os.path import isfile, join
mpl.rcParams['svg.fonttype'] = "none"
mpl.rcParams['font.size'] = 16
from matplotlib import pyplot as plt
import matplotlib.cm as cm


rf = np.loadtxt("data/boosting_cv_error_15000_trees.csv", delimiter=",")
bag = np.loadtxt("data/acc_out.csv", delimiter=",")

fig = plt.figure()
ax = fig.add_subplot(1,1,1)

ax.plot(np.linspace(0,len(rf), len(rf)), rf, "-", color="black", alpha=1.0, label="\$\\text{Random Forest}\$")
#ax.plot(np.linspace(0,len(rf), len(rf)), 1-bag, "-o", color="red", alpha=0.6, label="\$\\text{Bagging}\$")


ax.spines["right"].set_visible(False)
ax.spines["top"].set_visible(False)
ax.xaxis.set_ticks_position("bottom")
ax.yaxis.set_ticks_position("left")
ax.set_xlabel("\$\\text{Number\ of\ trees}\$")
ax.set_ylabel("\$\\text{CV_{error}}\$")
#ax.legend(loc="best", frameon=False, labelspacing=0.05)
plt.show()
