#!/usr/bin/env python3

# Canvas handling for the python+tk frontend. See README for notes.

from tkinter import *
from tkinter.filedialog import *
from tkinter.font import *

#!START local
from external import *
from program import *
#!END local

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
        self.seloutput = None
        self.objects = {}
        self.outputs = {}
        self.inputs = {}

    def useTool(self, tool):
        self.tool = tool
    
    def canvas_down(self, event):
        self.isdown = True
        self.dragdist = 0
        x = self.canvas.canvasx(event.x)
        y = self.canvas.canvasy(event.y)
        self.last_x, self.last_y = x, y

        if self.tool is not None:
            self.tool(self.program, x, y)
            self.tool = None
            self.update_display()
            return

        # TODO select whatever is here
        self.selected = self.obj_at(x, y)
        if self.selected is None and self.seloutput is None:
            # find out if an output was clicked
            self.seloutput = self.out_at(x, y)
        elif self.selected is None and self.seloutput:
            selinput = self.in_at(x, y)
            seloutput = self.seloutput
            self.seloutput = None
            if selinput:
                p = Pipe(seloutput, selinput)
                self.program.add_pipe(p)

        self.update_display()

    def canvas_up(self, event):
        self.isdown = False
        x = self.canvas.canvasx(event.x)
        y = self.canvas.canvasy(event.y)
        self.last_x, self.last_y = x, y

    def canvas_move(self, event):
        # TODO change cursor
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
        self.program.std_init()
        self.update_display()
        self.fname = None

    def loadProg(self):
        """ Load a program from a file. Called when the menu item File->Open is
        activated. """

        self.fname = askopenfilename(defaultextension=".evan", filetypes = FILETYPES)
        with open(self.fname) as f:
            self.program = program_from_json(f.read())
        self.update_display()

    def openProg(self, fname):
        """ Load a program fromt he given filename. """

        self.fname = fname
        with open(fname) as f:
            self.program = program_from_json(f.read())
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
        compile_prog(self)

    def do_run(self):
        """ Run the analysis. """

        self.saveProg()
        compile_prog(self)
        run_prog(self, None)

    def obj_at(self, x, y):
        """ Find and return the object at the specified co-ordinates. """

        ids = self.canvas.find_overlapping(x,y,x,y)
        for id in ids:
            if id in self.objects:
                return self.objects[id]

        return None

    def in_at(self, x, y):
        """ Find and return the object with an output at the specified co-ordinates. """
        
        ids = self.canvas.find_overlapping(x,y,x,y)
        for id in ids:
            if id in self.inputs:
                return self.inputs[id]

        return None

    def out_at(self, x, y):
        """ Find and return the object with an output at the specified co-ordinates. """
        
        ids = self.canvas.find_overlapping(x,y,x,y)
        for id in ids:
            if id in self.outputs:
                return self.outputs[id]

        return None

    def update_display(self):
        """ Update the canvas display. """

        # TODO don't re-draw /everything/ - use find_all and Graphical.ids

        self.canvas.delete(ALL)
        blocks, pipes, comments = self.program.blocks, self.program.pipes, self.program.comments

        for pipe in pipes:
            p = pipes[pipe]
            sn, si = p.source
            dn, di = p.dest
            sp, dp = blocks[sn].pos(), blocks[dn].pos()
            self.canvas.create_line(sp[0], sp[1], dp[0], dp[1])

        for block in blocks:
            # TODO get size of glyph or string
            h = 26
            w = 70
            b = blocks[block]
            pos = blocks[block].pos()
            i = self.canvas.create_rectangle(
                pos[0]-w/2, pos[1]-h/2, pos[0]+w/2, pos[1]+h/2,
                fill="#00ffff", activefill="#aaffff")
            self.objects[i] = b

            mh = 8
            # input and output blocks
            for i in range(0, b.input_count):
                iw = w/b.input_count
                o = self.canvas.create_rectangle(
                    pos[0]-w/2+i*iw,
                    pos[1]-h/2-mh,
                    pos[0]-w/2+(i+1)*iw,
                    pos[1]-h/2,
                    fill="#00ffff", activefill="#aaffff")
                self.inputs[o] = block, i

            for i in range(0, b.output_count):
                ow = w/b.output_count
                o = self.canvas.create_rectangle(
                    pos[0]-w/2+i*ow,
                    pos[1]+h/2,
                    pos[0]-w/2+(i+1)*ow,
                    pos[1]+h/2+mh,
                    fill="#00ffff", activefill="#aaffff")
                self.outputs[o] = block, i

            self.canvas.create_text(pos, font=FONTA,
                text=b.ident, state=DISABLED)

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
