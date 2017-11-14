import numpy as np
import sys
from os import listdir
from os.path import isfile, join
import os

path = sys.argv[1]
files = [f for f in listdir(path) if isfile(join(path, f)) and "res" in f and "tmp" not in f]

m=0
for filename in files:
    if(os.stat(path+"/"+filename).st_size < 200):
        continue
    print(filename)
    try:
        data = np.loadtxt(path+"/"+filename, delimiter=",")
        print(np.max(data))
        m += data
    except Exception as exc:
        print(exc)
        continue

m /= len(files)

np.savetxt("averaged_matrix.csv", m, delimiter=",")
