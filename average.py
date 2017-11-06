import numpy as np
import sys
from os import listdir
from os.path import isfile, join

path = sys.argv[1]
files = [f for f in listdir(path) if isfile(join(path, f)) and "res" in f]

m=0
for filename in files:
    print(filename)
    try:
        data = np.loadtxt(path+"/"+filename, delimiter=",")
        m += data
    except Exception as exc:
        print(exc)
        continue

m /= len(files)
m /= 100

np.savetxt("averaged_matrix.csv", m, delimiter=",")
