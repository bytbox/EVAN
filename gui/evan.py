#!/usr/bin/env python3

# Main component of the EVAN python+tk frontend. See README for notes.

from optparse import *

from tkinter import *
from tkinter.filedialog import *

from canvas import *
from external import *
from program import *

# parse options
parser = OptionParser(usage="usage: %prog [fname.evan]")
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
    menubar.add_cascade(label="File", menu=filemenu)
    filemenu.add_command(label="New", command=cState.newProg)
    filemenu.add_command(label="Open...", command=cState.loadProg)
    filemenu.add_command(label="Save", command=cState.saveProg)
    filemenu.add_command(label="Save As...", command=cState.saveProgAs)
    filemenu.add_separator()
    filemenu.add_command(label="Exit", command=root.destroy)

    # Help menu
    helpmenu = Menu(menubar, tearoff=0)
    menubar.add_cascade(label="Help", menu=helpmenu)
    helpmenu.add_command(label="About", command=showAbout)

def showAbout():
    pass

class CommentTool:
    """ The comment tool. """

    pass

class BlockTool:
    """ The block drawing tool. """

    pass

tools = {
    "Cancel": None,
    "Comment": CommentTool,
}

def use_tool(t, cState):
    return lambda:cState.useTool(t)

def populate_toolbar(toolbar, cState):
    """ Populate toolbar. """

    Button(toolbar, text="Compile", command=cState.do_compile).pack(side=TOP)
    Button(toolbar, text="Run", command=cState.do_run).pack(side=TOP)

    for tool in tools:
        b = Button(toolbar, text=tool, command=use_tool(tool, cState))
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
cState = CanvasState(canvas)
# And event hooks
canvas.bind('<ButtonPress>', cState.canvas_down)
canvas.bind('<ButtonRelease>', cState.canvas_up)
canvas.bind('<Motion>', cState.canvas_move)

# canvas should be scrollable, in both directions
hBar = Scrollbar(drawFrame, orient=HORIZONTAL)
hBar.pack(side=BOTTOM, anchor='se', fill=X)
hBar.config(command=canvas.xview)
canvas.config(xscrollcommand=hBar.set)

vBar = Scrollbar(extraFrame, orient=VERTICAL)
vBar.pack(side=RIGHT, anchor='ne', fill=Y)
vBar.config(command=canvas.yview)
canvas.config(yscrollcommand=vBar.set)

# Toolbar. Yes, the toolbar goes on the left side; not the top, not the right,
# and not the bottom. Screens are wider than they are tall, and people will
# tend to be working on the left side of the screen, making a left-handed
# toolbar easier to use. This shouldn't even be configurable - any attempt to
# change it is probably a mistake.
toolbarFrame = Frame(root, bd=2, relief=SUNKEN)
toolbarFrame.pack(side=LEFT, anchor='nw', fill=BOTH, expand=0)
populate_toolbar(toolbarFrame, cState)

# Menu bar
make_menubar(root)

# Initialize.
if arg_progname is None:
    cState.newProg()
else:
    cState.openProg(arg_progname)

# Main loop
root.mainloop()
