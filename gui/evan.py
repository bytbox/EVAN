#!/usr/bin/env python3

# Main component of the EVAN python+tk frontend. See README for notes.

from tkinter import *
from tkinter.filedialog import *

from program import *

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

class ReadyTool:
    """ The default tool, which draws pipes when appropriate and otherwise acts
    just like SelectTool. """

    def __init__(self):
        pass

    def select(self):
        pass

class SelectTool:
    """ The selection tool, which selects whatever it's clicked on, pulling up
    context menus on right clicks. """

    def __init__(self):
        pass

    def select(self):
        pass

class CommentTool:
    """ The comment tool. """

class BlockTool:
    """ The block drawing tool. """

    pass

tools = {
    "Cancel": ReadyTool,
    "Select": SelectTool,
}

def use_tool(t, cState):
    return lambda:cState.useTool(t)

def populate_toolbar(toolbar, cState):
    """ Populate toolbar. """
    for tool in tools:
        b = Button(toolbar, text=tool, command=use_tool(tool, cState))
        b.pack(side=TOP)

class CanvasState:
    """ Encapsulates the state of the canvas as well as the user interaction
    logic. """

    def __init__(self, canvas):
        self.last_x = -1
        self.last_y = -1
        self.isdown = False
        self.dragdist = 0
        self.tool = None
        self.canvas = canvas
        self.program = Program()

    def useTool(self, toolName):
        print(toolName)
    
    def canvas_down(self, event):
        self.isdown = True
        self.dragdist = 0
        x = self.canvas.canvasx(event.x)
        y = self.canvas.canvasy(event.y)
        self.last_x, self.last_y = x, y

    def canvas_up(self, event):
        self.isdown = False
        x = self.canvas.canvasx(event.x)
        y = self.canvas.canvasy(event.y)
        self.last_x, self.last_y = x, y

    def canvas_move(self, event):
        x = self.canvas.canvasx(event.x)
        y = self.canvas.canvasy(event.y)
        if self.isdown:
            gId = self.canvas.create_line(self.last_x,self.last_y,x,y)
        self.last_x, self.last_y = x, y

    def newProg(self):
        """ Create a new program. Called when the menu item File->New is activated.
        """

        self.program = Program()
        self.update_display()
        self.fname = None

    def loadProg(self):
        """ Load a program from a file. Called when the menu item File->Open is
        activated. """

        pass

    def doLoadProg(self, j):
        """ Load a program from the given JSON string. """
        self.program = Program_from_json(j)
        self.update_display()

    def saveProg(self):
        """ Save a program to a file. Called when the menu item File->Save is
        activated. """

        if self.fname is None:
            self.fname = asksaveasfilename()
        with open(self.fname, 'w') as f:
            f.write(self.program.as_json())

    def saveProgAs(self):
        """ Save a program to a selected file. Called when the menu item File->Save
        As is activated. """
        
        self.fname = asksaveasfilename()
        self.saveProg

    def update_display(self):
        """ Update the canvas display. """

        # TODO don't re-draw /everything/ - use find_all and Graphical.ids

        self.canvas.delete(ALL)
        blocks, pipes = self.program.blocks, self.program.pipes
        for block in blocks:
            canvas.create_text(blocks[block].pos, text=block)

        for pipe in pipes:
            pass

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

# Initialize
cState.newProg()

# Main loop
root.mainloop()
