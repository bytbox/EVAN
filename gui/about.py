# About dialog for the EVAN python+tk frontend. See README for notes.

from tkinter import *

class AboutDialog(Toplevel):
    def __init__(self, parent):
        Toplevel.__init__(self, parent)
        self.title = "About EVAN"

about_dialog_parent = None

def prepareAbout(parent):
    global about_dialog_parent
    about_dialog_parent = parent

def showAbout():
    ad = AboutDialog(about_dialog_parent)
