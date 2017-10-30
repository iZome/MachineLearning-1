import numpy as np
import matplotlib as mpl
import pylab as plt
import sys
from os import listdir
from os.path import isfile, join
mpl.rcParams['svg.fonttype'] = "none"
mpl.rcParams['font.size'] = 16
from matplotlib import pyplot as plt

colors = ["#d7191c", "#fdae61", "#a6d96a", "#1a9641"]


x = np.loadtxt("data/x_task1i.csv", delimiter=",")
y = np.loadtxt("data/y_task1i.csv", delimiter=",")

linspace = np.linspace(-1, 1, len(x))
x_lin = np.linspace(-1,1,len(x))

fig = plt.figure()
ax = fig.add_subplot(1,1,1)

#ax.scatter(linspace, y, color=colors[2], label="\$y = 0.8x + \epsilon(x)\$")
ax.plot(linspace, 0.8*x_lin+0.5, color=colors[1], label="\$y= 0.5 + 0.8x\$")
ax.plot(linspace, 0.8*x_lin-0.5, color=colors[0], label="\$y= -0.5 + 0.8x\$")
ax.plot(linspace, 0.8*x_lin, color=colors[3], label="\$y= 0.8x\$")

ax.spines["right"].set_visible(False)
ax.spines["top"].set_visible(False)
ax.xaxis.set_ticks_position("bottom")
ax.yaxis.set_ticks_position("left")
ax.set_xlabel("\$x$")
ax.set_ylabel("\$y\$")
ax.legend(loc="best", frameon=False, labelspacing=0.05)
plt.xlim(-1,1)
plt.show()
