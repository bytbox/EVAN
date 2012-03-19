#!/usr/bin/env python3

# Main component of the EVAN python+tk frontend. See README for notes.

from optparse import *
import random

from tkinter import *
from tkinter.filedialog import *

#!START local
from about import *
from canvas import *
from docs import *
from program import *
from tools import *
#!END local

VERSION = '0.1'

# parse options
parser = OptionParser(usage="usage: %prog [fname.evan]", version="EVAN "+VERSION)
options, args = parser.parse_args()

arg_progname = None
if len(args) > 1:
    parser.print_help()
    exit(1)
if len(args) == 1:
    arg_progname = args[0]

def make_menubar(root):
    """ Initializes the menus."""

    menubar = Menu(root)
    root.config(menu=menubar)
    
    # File menu
    filemenu = Menu(menubar, tearoff=0)
    menubar.add_cascade(label="File", underline=0, menu=filemenu)
    filemenu.add_command(label="New", underline=0, command=cState.newProg)
    filemenu.add_command(label="Open...", underline=0, command=cState.loadProg)
    filemenu.add_command(label="Save", underline=0, command=cState.saveProg)
    filemenu.add_command(label="Save As...", underline=5, command=cState.saveProgAs)
    filemenu.add_separator()
    filemenu.add_command(label="Exit", underline=1, command=root.destroy)

    # Help menu
    helpmenu = Menu(menubar, tearoff=0)
    menubar.add_cascade(label="Help", underline=0, menu=helpmenu)
    helpmenu.add_command(label="About", underline=0, command=showAbout)

def use_tool(t, cState):
    return lambda:cState.useTool(t)

# A popup toolbar.
class Popup(Menu):

    def __init__(self, cState, root, tb, cname, cat):
        Menu.__init__(self, root, tearoff=0)
        self.tb = tb
        self.up = False
        for n in cat:
            self.add_command(label=n, command=use_tool(cat[n], cState))
    
    def popup(self):
        if self.up:
            self.unpost()
            self.up = False
        else:
            self.up = True
            try:
                self.post(self.tb.x, self.tb.y)
            finally:
                self.grab_release()

# Toolbar. Yes, the toolbar goes on the left side; not the top, not the right,
# and not the bottom. Screens are wider than they are tall, and people will
# tend to be working on the left side of the screen, making a left-handed
# toolbar easier to use. This shouldn't even be configurable - any attempt to
# change it is probably a mistake.
class Toolbar(Frame):
    def __init__(self, cState, root):
        Frame.__init__(self, root, bd=2, relief=SUNKEN)
        self.pack(side=LEFT, anchor='nw', fill=BOTH, expand=0)
        self.x = 0
        self.y = 0

    def move(self, e):
        self.x, self.y = e.x_root, e.y_root

    def populate(self):
        Button(self, text="Compile", command=cState.do_compile).pack(side=TOP)
        Button(self, text="Run", command=cState.do_run).pack(side=TOP)
        Button(self, text="Run On...", command=cState.do_run_on).pack(side=TOP)
        for tool in tools:
            b = Button(self, text=tool, command=use_tool(tools[tool], cState))
            b.pack(side=TOP)

        for cat in categories:
            p = Popup(cState, root, self, cat, categories[cat])
            b = Button(self, text=cat, command=p.popup)
            b.pack(side=TOP)

# Set up the GUI
root = Tk()
root.title('EVAN')
root.protocol('WM_DELETE_WINDOW', root.destroy)

# Drawing area
drawFrame = Frame(root)
drawFrame.pack(side=RIGHT, anchor='ne', fill=BOTH, expand=1)
extraFrame = Frame(drawFrame)
extraFrame.pack(side=TOP, anchor='nw', fill=BOTH, expand=1)
canvas = Canvas(extraFrame, bg='white', bd=2, relief=SUNKEN)
canvas.pack(side=LEFT, anchor='nw', fill=BOTH, expand=1)
cState = CanvasState(canvas, root)
# And event hooks
canvas.bind('<ButtonPress>', cState.canvas_down)
canvas.bind('<ButtonRelease>', cState.canvas_up)
canvas.bind('<Motion>', cState.canvas_move)
root.bind('<x>', cState.dele)
root.bind('<Control-n>', cState.newProg)
root.bind('<Control-o>', cState.loadProg)
root.bind('<Control-s>', cState.saveProg)

# canvas should be scrollable, in both directions
hBar = Scrollbar(drawFrame, orient=HORIZONTAL)
hBar.pack(side=BOTTOM, anchor='se', fill=X)
hBar.config(command=canvas.xview)
canvas.config(xscrollcommand=hBar.set)

vBar = Scrollbar(extraFrame, orient=VERTICAL)
vBar.pack(side=RIGHT, anchor='ne', fill=Y)
vBar.config(command=canvas.yview)
canvas.config(yscrollcommand=vBar.set)

toolbar = Toolbar(cState, root)
toolbar.populate()
root.bind('<Motion>', toolbar.move)

# Menu bar
make_menubar(root)

# Initialize.
if arg_progname is None:
    cState.newProg()
else:
    cState.openProg(arg_progname)

prepareAbout(root)

# Main loop
root.mainloop()
