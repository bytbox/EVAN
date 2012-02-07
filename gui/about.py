# About dialog for the EVAN python+tk frontend. See README for notes.

from tkinter import *

VERSION = '0.1'

class AboutDialog(Toplevel):
    def __init__(self, parent):
        Toplevel.__init__(self, parent)
        self.parent = parent
        self.title("About EVAN")
        #self.transient(parent)
        body = Frame(self)
        Label(body, text="EVAN "+VERSION).pack()
        self.initial_focus = body
        body.pack(padx=5, pady=5)
        self.grab_set()
        self.protocol("WM_DELETE_WINDOW", self.cancel)
        self.geometry("+%d+%d" % (parent.winfo_rootx()+50,
            parent.winfo_rooty()+50))
        self.initial_focus.focus_set()
        self.wait_window(self)

    def cancel(self):
        self.parent.focus_set()
        self.destroy()

    def validate(self):
        return 1

    def apply(self):
        pass

about_dialog_parent = None

def prepareAbout(parent):
    global about_dialog_parent
    about_dialog_parent = parent

def showAbout():
    ad = AboutDialog(about_dialog_parent)
