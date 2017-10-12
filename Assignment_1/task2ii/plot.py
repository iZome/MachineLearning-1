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
im = ax.imshow( m, cmap=plt.cm.Reds,   interpolation='none', \
extent=[20,m.shape[0]+20,1,40], aspect='auto', \
vmax=np.log(m.max()), vmin=-np.log(m.max())
)
plt.colorbar(im)

plt.show()
