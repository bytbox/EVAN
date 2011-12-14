#!/usr/bin/env python3

# Main component of the EVAN python+tk frontend. See README for notes.

from tkinter import *

def make_menubar(root):
    menubar = Menu(root)
    root.config(menu=menubar)
    
    # File menu
    filemenu = Menu(menubar, tearoff=0)
    menubar.add_cascade(label="File", menu=filemenu)

    # Help menu
    helpmenu = Menu(menubar, tearoff=0)
    menubar.add_cascade(label="Help", menu=helpmenu)

# Mouse handling info
last_x = 0
last_y = 0
isdown = False

def canvas_down(event):
    global isdown, last_x, last_y, canvas
    isdown = True
    x = canvas.canvasx(event.x)
    y = canvas.canvasy(event.y)

def canvas_up(event):
    global isdown, last_x, last_y, canvas
    isdown = False
    x = canvas.canvasx(event.x)
    y = canvas.canvasy(event.y)

def canvas_move(event):
    global isdown, last_x, last_y, canvas
    x = canvas.canvasx(event.x)
    y = canvas.canvasy(event.y)
    if isdown:
        canvas.create_line(last_x,last_y,x,y)
    last_x = x
    last_y = y

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
canvas.bind('<ButtonPress>', canvas_down)
canvas.bind('<ButtonRelease>', canvas_up)
canvas.bind('<Motion>', canvas_move)

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
