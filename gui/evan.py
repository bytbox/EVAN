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
    """ The default tool, which draws pipes when appropriate and otherwise
    selects and drags. """

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
}

def use_tool(t, cState):
    return lambda:cState.useTool(t)

def populate_toolbar(toolbar, cState):
    """ Populate toolbar. """

    Button(toolbar, text="Compile", command=cState.do_compile).pack(side=TOP)
    Button(toolbar, text="Run", command=cState.do_run).pack(side=TOP)

    cState.useTool(ReadyTool)

    for tool in tools:
        b = Button(toolbar, text=tool, command=use_tool(tool, cState))
        b.pack(side=TOP)

FILETYPES = [("EVAN Programs", ".evan")]

class CanvasState:
    """ Encapsulates the state of the canvas as well as the user interaction
    logic. """

    def __init__(self, canvas):
        self.last_x = -1
        self.last_y = -1
        self.isdown = False
        self.dragdist = 0
        self.canvas = canvas
        self.program = None
        self.tool = ReadyTool
        self.selected = None
        self.objects = {}

    def useTool(self, tool):
        self.tool = tool
    
    def canvas_down(self, event):
        self.isdown = True
        self.dragdist = 0
        x = self.canvas.canvasx(event.x)
        y = self.canvas.canvasy(event.y)
        self.last_x, self.last_y = x, y
        # TODO select whatever is here
        self.selected = self.obj_at(x, y)
        self.update_display()

    def canvas_up(self, event):
        self.isdown = False
        x = self.canvas.canvasx(event.x)
        y = self.canvas.canvasy(event.y)
        self.last_x, self.last_y = x, y

    def canvas_move(self, event):
        x = self.canvas.canvasx(event.x)
        y = self.canvas.canvasy(event.y)
        if self.isdown and self.selected:
            dx = x - self.last_x
            dy = y - self.last_y
            self.selected.move(dx, dy)
        self.last_x, self.last_y = x, y
        self.update_display()

    def newProg(self):
        """ Create a new program. Called when the menu item File->New is activated.
        """

        self.program = Program()
        self.update_display()
        self.fname = None

    def loadProg(self):
        """ Load a program from a file. Called when the menu item File->Open is
        activated. """

        self.fname = askopenfilename(defaultextension=".evan", filetypes = FILETYPES)
        self.program = program_from_json(self.fname)
        self.update_display()

    def saveProg(self):
        """ Save a program to a file. Called when the menu item File->Save is
        activated. """

        if self.fname is None:
            self.fname = asksaveasfilename(defaultextension=".evan", filetypes = FILETYPES)
        with open(self.fname, 'w') as f:
            f.write(self.program.as_json())

    def saveProgAs(self):
        """ Save a program to a selected file. Called when the menu item File->Save
        As is activated. """
        
        self.fname = asksaveasfilename(defaultextension=".evan", filetypes = FILETYPES)
        self.saveProg

    def do_compile(self):
        """ Perform compilation. """

        self.saveProg()

    def do_run(self):
        """ Run the analysis. """

        self.do_compile()

    def obj_at(self, x, y):
        """ Find and return the object at the specified co-ordinates. """

        ids = self.canvas.find_overlapping(x,y,x,y)
        for id in ids:
            if id in self.objects:
                return self.objects[id]

        return None

    def update_display(self):
        """ Update the canvas display. """

        # TODO don't re-draw /everything/ - use find_all and Graphical.ids

        self.canvas.delete(ALL)
        blocks, pipes, comments = self.program.blocks, self.program.pipes, self.program.comments
        for block in blocks:
            h = 26
            w = 50
            pos = blocks[block].pos()
            i = self.canvas.create_rectangle(
                pos[0]-w/2, pos[1]-h/2, pos[0]+w/2, pos[1]+h/2,
                fill="#00ffff", activefill="#aaffff")
            self.objects[i] = blocks[block]
            self.canvas.create_text(pos,
                text=block, state=DISABLED)

        for pipe in pipes:
            pass

        for cid in comments:
            comment = comments[cid]
            h = 26
            w = 160
            pos = comment.pos()
            ids = []
            ids.append(canvas.create_rectangle(
                pos[0]-w/2, pos[1]-h/2, pos[0]+w/2, pos[1]+h/2,
                fill="#ffff00", activefill="#ffffaa"))
            ids.append(canvas.create_text(pos,
                text=comment.text, state=DISABLED, width=w))

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
