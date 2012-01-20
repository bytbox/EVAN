# Displaying results for the python+tk frontend. See README for notes.

from tkinter import *

def display_results(r):
    """ Displays the given results. """

    root = Tk()
    root.title('Results')
    root.protocol('WM_DELETE_WINDOW', root.destroy)

    resFrame = Frame(root)
    resFrame.pack(side=LEFT, anchor='nw', fill=BOTH, expand=1)

    resDisp = Canvas(resFrame, bg='white', bd=2, relief=SUNKEN)
    resDisp.pack(side=LEFT, anchor='nw', fill=BOTH, expand=1)
    if isinstance(r, list):
        # render a histogram. TODO don't make quite so many assumptions
        print(r)
    else
        resDisp.create_text(20, 20, text=r)
