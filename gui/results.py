# Displaying results for the python+tk frontend. See README for notes.

import json
from tkinter import *

def display_results(rs):
    """ Displays the given results. """

    r = json.loads(rs)
    if isinstance(r, str):
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

