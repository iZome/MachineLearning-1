import numpy as np
import matplotlib as mpl
import pylab as plt
import sys
from os import listdir
from os.path import isfile, join
mpl.rcParams['svg.fonttype'] = "none"
mpl.rcParams['font.size'] = 16
from matplotlib import pyplot as plt

path = sys.argv[1]

files = [f for f in listdir(path) if isfile(join(path, f)) and "matrix" in f]
print(len(files))

fig, ax = plt.subplots()
m=0
for filename in files:
    print(path+"/"+filename)
    data = np.loadtxt(path+"/"+filename, delimiter=",")
    m += data

m = np.rot90(np.transpose(m/len(files)), k=3)

print(m)
norm = mpl.colors.Normalize(vmin=-5,vmax=5)

im = ax.imshow( m, cmap=plt.cm.Reds, interpolation='none', norm=norm, \
extent=[20,m.shape[0]+20, 0.2,1.1], aspect='auto', \
#vmax=2, vmin=m.min()
)
ax.set_xlabel("\$N$")
ax.set_ylabel("\$\sigma\$")
#ax.spines["right"].set_visible(False)
#ax.spines["top"].set_visible(False)
#ax.spines["left"].set_visible(False)
#ax.spines["bottom"].set_visible(False)

ax.xaxis.set_ticks_position('none')
ax.yaxis.set_ticks_position('none')

cbar = plt.colorbar(im)
#cbar.ax.set_title("\$\ln(max_{x,y})\$")

plt.show()
