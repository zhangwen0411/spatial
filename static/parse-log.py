#!/usr/bin/env python

import os, sys, datetime

def pad(i):
  s = str(i)
  while len(s) < 4:
    s = "0" + s
  return s

def main():
  logDir = os.getcwd() + "/logs/"
  if not os.path.exists(logDir):
    os.makedirs(logDir)
  else:
    os.system("rm " + logDir + "/*.log")

  traversal = 1
  out = open(logDir + pad(0) + "_Heading.log", "w")

  with open("compile.log", "r") as f:
    lines = f.readlines()
    i = 0
    while i < len(lines):
      line = lines[i]
      if "Starting traversal" in line:
        out.close()
        traversalName = line[19:].replace('\n', '').replace('\r', '')
        out = open(logDir + pad(traversal) + "_" + traversalName + ".log", "w")
        traversal += 1
      elif line.startswith("Exception"):
        out.close()
        out = open(logDir + pad(traversal) + "_Exception.log", "w")
        traversal += 1

      out.write(line)
      i += 1

  out.close()

if __name__ == "__main__":
    main()