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
    n, bins, patches = ax.hist(r, 25, facecolor='green', alpha=0.75)
    ax.grid(True)
    plt.show()
