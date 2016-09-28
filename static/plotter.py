#!/bin/python


import numpy as np
import csv
import getopt, sys
import matplotlib.pyplot as plt
import pylab
from StringIO import StringIO
import pandas as pd

app_name = sys.argv[1]
wiki_home = sys.argv[2]
csv_name = wiki_home + app_name + ".csv"
png_name = wiki_home + app_name + ".png"

df = pd.read_csv(csv_name, delimiter=";")
x = list(df['time'].index)
times = list(df['time'].values)
colors = list(df['color'].values)
commits = list(df['commit'].values)

plt.scatter(x, times, c=colors)
plt.xticks(x, commits, rotation=30)
plt.gcf().subplots_adjust(bottom=0.15)
plt.xlabel("Commit")
plt.ylabel("Cycles")
plt.title("Estimated Cycles for " + app_name)
pylab.savefig(png_name)

