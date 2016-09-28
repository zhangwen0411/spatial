#!/bin/python


import numpy as np
import csv
import getopt, sys
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import pylab
from StringIO import StringIO
import pandas as pd

app_name = sys.argv[1]
wiki_home = sys.argv[2]
csv_name = wiki_home + app_name + ".csv"
png_name = wiki_home + app_name + ".png"

df = pd.read_csv(csv_name, delimiter=";")
x = list(df.ix[:,0].index)
times = list(df.ix[:,1].values)
colors = list(df.ix[:,0].values)
commits = list(df.ix[:,2].values)

fig, ax = plt.subplots()
ax=plt.gca()
ax.ticklabel_format(useOffset=False)
plt.scatter(x, times, c=colors)
plt.xticks(x, commits, rotation=30)
plt.gcf().subplots_adjust(bottom=0.15, left=0.2)
plt.xlabel("Commit")
plt.ylabel("Cycles")
plt.title("Estimated Cycles for " + app_name)
pylab.savefig(png_name)

