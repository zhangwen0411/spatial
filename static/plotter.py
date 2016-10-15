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
from matplotlib.ticker import ScalarFormatter 
import os

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
plt.scatter(x, times, c=colors, s=100)
plt.xticks(x, commits, rotation=60)
plt.gcf().subplots_adjust(bottom=0.2, left=0.2)
ax=plt.gca()
# ax.ticklabel_format(useOffset=False)
fig.set_size_inches(38.5, 20.5)
ax.set_ylim([-max(times)*0.01, (max(times)*(1.01))])
ax.set_xlim([-1, len(x)])
plt.xlabel("Commit")
plt.ylabel("Cycles")
date = os.popen("date").read()
plt.title("Estimated Cycles for " + app_name + ", " + date)
pylab.savefig(png_name)

