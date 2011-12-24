#!/usr/bin/env python3

# Canvas handling for the python+tk frontend. See README for notes.

from tkinter import *
from tkinter.filedialog import *
from tkinter.font import *

from program import *

FILETYPES = [("EVAN Programs", ".evan")]

FONTA = ("Helvetica", 10, "bold")
FONTB = ("Times", 10)

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
        self.tool = None
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
            b = blocks[block]
            pos = blocks[block].pos()
            i = self.canvas.create_rectangle(
                pos[0]-w/2, pos[1]-h/2, pos[0]+w/2, pos[1]+h/2,
                fill="#00ffff", activefill="#aaffff")
            self.objects[i] = blocks[block]

            # input and output blocks
            if b.input_count > 0:
                pass

            if b.output_count > 0:
                pass

            self.canvas.create_text(pos, font=FONTA,
                text=block, state=DISABLED)

        for pipe in pipes:
            pass

        for cid in comments:
            comment = comments[cid]
            h = 26
            w = 160
            pos = comment.pos()
            i = self.canvas.create_rectangle(
                pos[0]-w/2, pos[1]-h/2, pos[0]+w/2, pos[1]+h/2,
                fill="#ffff00", activefill="#ffffaa")
            self.objects[i] = comment
            self.canvas.create_text(pos, font=FONTB,
                text=comment.text, state=DISABLED, width=w)
