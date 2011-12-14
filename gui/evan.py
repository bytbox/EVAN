#!/usr/bin/env python3

# Main component of the EVAN python+tk frontend. See README for notes.

from tkinter import *

from program import *

def make_menubar(root):
    """ Initializes the menus."""
    menubar = Menu(root)
    root.config(menu=menubar)
    
    # File menu
    filemenu = Menu(menubar, tearoff=0)
    menubar.add_cascade(label="File", menu=filemenu)

    # Help menu
    helpmenu = Menu(menubar, tearoff=0)
    menubar.add_cascade(label="Help", menu=helpmenu)

class CanvasState:
    """ Encapsulates the state of the canvas as well as the user interaction
    logic. """

    def __init__(self, canvas):
        self.last_x = -1
        self.last_y = -1
        self.isdown = False
        self.dragdist = 0
        self.canvas = canvas
    
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

# Set up the GUI
root = Tk()
root.title('EVAN')
root.protocol('WM_DELETE_WINDOW', root.destroy)

# Menu bar
make_menubar(root)

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

# Main loop
root.mainloop()
