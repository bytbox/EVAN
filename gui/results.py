# Displaying results for the python+tk frontend. See README for notes.

from tkinter import *

def display_results(r):
    """ Displays the given results. """

    root = Tk()
    root.title('Results')
    root.protocol('WM_DELETE_WINDOW', root.destroy)

    print(r)
