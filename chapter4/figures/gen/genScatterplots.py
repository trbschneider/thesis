#!/usr/bin/env python
"""
You can use TeX to render all of your matplotlib text if the rc
parameter text.usetex is set.  This works currently on the agg and ps
backends, and requires that you have tex and the other dependencies
described at http://matplotlib.sf.net/matplotlib.texmanager.html
properly installed on your system.  The first time you run a script
you will see a lot of output from tex and associated tools.  The next
time, the run may be silent, as a lot of the information is cached in
~/.tex.cache

"""
from pylab import *
from numpy import *
from sys import argv
from os.path import splitext

table = genfromtxt(argv[1], unpack=True)
basename, extension = splitext(argv[1])
factor=float(argv[2])
tit=argv[3]
color=argv[4]



rc('text', usetex=True)

figure(figsize=(4,4),dpi=300)

scatter(table[1,:],table[0,:]*factor, marker='o', edgecolor=color, facecolor='none', s=1)

xlim(0,1)
ylim(0,None)


xlabel('$d/\max (d)$')
ylabel('')
title(tit)

savefig('%s.pdf'%basename)


