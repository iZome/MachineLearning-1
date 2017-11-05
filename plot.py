import numpy as np
import matplotlib as mpl
import pylab as plt
import sys
from os import listdir
from os.path import isfile, join
mpl.rcParams['svg.fonttype'] = "none"
mpl.rcParams['font.size'] = 16
from matplotlib import pyplot as plt


#y = np.loadtxt("y.csv", delimiter=",")
#x = np.loadtxt("x.csv", delimiter=",")
#h2 = np.loadtxt("h2.csv", delimiter=",")
#h10 = np.loadtxt("h10.csv", delimiter=",")
#target = np.loadtxt("target.csv", delimiter=",")
#c = np.loadtxt("temp.csv", delimiter=",")

p0 = np.loadtxt("p1.csv", delimiter=",")





fig = plt.figure()
ax = fig.add_subplot(1,1,1)

#ax.plot(x, h10, color="black")
ax.scatter(np.linspace(-1,1,100), p0, color="blue")
#ax.scatter(x, y, label="\$E_{val}\$", color="red")
#ax.plot(x, target, label="\$E_{val}\$", color="pink")
#ax.plot(x, c, label="\$E_{val}\$", color="orange")



ax.spines["right"].set_visible(False)
ax.spines["top"].set_visible(False)
ax.xaxis.set_ticks_position("bottom")
ax.yaxis.set_ticks_position("left")
plt.show()
