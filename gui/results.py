# Displaying results for the python+tk frontend. See README for notes.

import json
from tkinter import *

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.mlab as mlab

def display_results(rs):
    """ Displays the given results. """

    r = json.loads(rs)
    if isinstance(r, list):
        # render a histogram. TODO don't make quite so many assumptions
        histogram(r)
    elif isinstance(r, str):
        root = Tk()
        root.title('Results')
        root.protocol('WM_DELETE_WINDOW', root.destroy)

        resFrame = Frame(root)
        resFrame.pack(side=LEFT, anchor='nw', fill=BOTH, expand=1)

        resDisp = Canvas(resFrame, bg='white', bd=2, relief=SUNKEN)
        resDisp.pack(side=LEFT, anchor='nw', fill=BOTH, expand=1)
        resDisp.create_text(20, 20, text=r)
    else:
        print(r)

def histogram(r):
    fig = plt.figure()
    ax = fig.add_subplot(111)

    # the histogram of the data
    n, bins, patches = ax.hist(r, 50, facecolor='green', alpha=0.75)

    # hist uses np.histogram under the hood to create 'n' and 'bins'.
    # np.histogram returns the bin edges, so there will be 50 probability
    # density values in n, 51 bin edges in bins and 50 patches.  To get
    # everything lined up, we'll compute the bin centers
    bincenters = 0.5*(bins[1:]+bins[:-1])

    ax.set_xlim(0, 1000)
    ax.set_ylim(0, 200)
    ax.grid(True)

    plt.show()
