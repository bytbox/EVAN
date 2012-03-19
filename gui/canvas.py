# Canvas handling for the python+tk frontend. See README for notes.

from tkinter import *
from tkinter.filedialog import *
from tkinter.font import *

#!START local
from external import *
from program import *
from results import *
#!END local

FILETYPES = [("EVAN Programs", ".evan")]
DATATYPES = [("HEPEVT", ".dat"), ("LHE", ".lhe")]

FONTA = ("Helvetica", 10, "bold")
FONTB = ("Times", 10)

BLOCK_HEIGHT = 26
PORT_HEIGHT = 10

class CanvasState:
    """ Encapsulates the state of the canvas as well as the user interaction
    logic. """

    def __init__(self, canvas, root):
        self.last_x = -1
        self.last_y = -1
        self.isdown = False
        self.dragdist = 0
        self.canvas = canvas
        self.root = root
        self.program = None
        self.tool = None
        self.selname = None
        self.selected = None
        self.seloutput = None
        self.selinput = None
        self.objectsById = {}
        self.outputs = {}
        self.inputs = {}
        self.datafname = None

    def useTool(self, tool):
        self.tool = tool
    
    def canvas_down(self, event):
        self.isdown = True
        self.dragdist = 0
        x = self.canvas.canvasx(event.x)
        y = self.canvas.canvasy(event.y)
        self.last_x, self.last_y = x, y

        if self.tool is not None:
            self.tool(self.program, x, y, self.root)
            self.tool = None
            self.update_display()
            return

        self.selname, self.selected = self.obj_at(x, y)
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

    def dele(self, event=None):
        self.program.delete(self.selname)
        self.update_display()

    def newProg(self, event=None):
        """ Create a new program. Called when the menu item File->New is activated.
        """

        self.program = Program()
        self.program.std_init()
        self.update_display()
        self.fname = None

    def loadProg(self, event=None):
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

    def saveProg(self, event=None):
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

        if self.datafname is None:
            self.do_run_on()
            return
        self.saveProg()
        if compile_prog(self):
            r = run_prog(self, self.datafname)
            display_results(r)

    def do_run_on(self):
        """ Ask for a filename and run the analysis on that file. """
        
        self.saveProg()
        if compile_prog(self):
            self.datafname = askopenfilename(filetypes = DATATYPES)
            r = run_prog(self, self.datafname)
            display_results(r)

    def obj_at(self, x, y):
        """ Find and return the object at the specified co-ordinates. """
       
        if self.in_at(x, y) is not None or self.out_at(x, y) is not None:
            return None, None

        for i in range(0,5):
            ids = self.canvas.find_overlapping(x-i,y-i,x+i,y+i)
            for id in ids:
                if id in self.objectsById:
                    return self.objectsById[id]

        return None, None

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

    def block_width(self, b):
        return 70

    def update_display(self):
        """ Update the canvas display. """

        # TODO don't re-draw /everything/ - use find_all and Graphical.ids

        self.canvas.delete(ALL)
        objects = self.program.objects

        if self.selected is None and self.seloutput:
            # We're in the process of drawing a pipe
            w = self.block_width(objects[self.seloutput[0]])
            oc = objects[self.seloutput[0]].output_count
            o = self.seloutput[1]
            sp = objects[self.seloutput[0]].pos()
            self.canvas.create_line(
                sp[0] - w/2 + (w/oc)/2 + o*(w/oc),
                sp[1]+BLOCK_HEIGHT/2+PORT_HEIGHT,
                self.last_x,
                self.last_y)

        for obj in objects:
            o = objects[obj]
            if o.kind == PIPE:
                sn, si = o.source
                dn, di = o.dest
                sp, dp = objects[sn].pos(), objects[dn].pos()
                sw = self.block_width(objects[sn])
                oc = objects[sn].output_count
                on = si
                dw = self.block_width(objects[dn])
                ic = objects[dn].input_count
                i = di
                color = "#000000"
                if o is self.selected:
                    color = "#ff2255"
                ld = self.canvas.create_line(
                    sp[0] - sw/2 + (sw/oc)/2 + on*sw/oc,
                    sp[1]+BLOCK_HEIGHT/2+PORT_HEIGHT,
                    dp[0] - dw/2 + (dw/ic)/2 + i*dw/ic,
                    dp[1]-BLOCK_HEIGHT/2-PORT_HEIGHT,
                    fill=color)
                self.objectsById[ld] = obj, o

        for obj in objects:
            o = objects[obj]
            if o.kind == BLOCK:
                b=o
                fill = "#00ffff"
                afill = "#aaffff"
                if o is self.selected:
                    fill = "#ffff00"
                    afill = "#ffffaa"
                # TODO get size of glyph or string
                h = BLOCK_HEIGHT
                w = self.block_width(o)
                pos = o.pos()
                i = self.canvas.create_rectangle(
                    pos[0]-w/2, pos[1]-h/2, pos[0]+w/2, pos[1]+h/2,
                    fill=fill, activefill=afill)
                self.objectsById[i] = obj, o

                mh = PORT_HEIGHT
                # input and output blocks
                for i in range(0, b.input_count):
                    iw = w/b.input_count
                    ob = self.canvas.create_rectangle(
                        pos[0]-w/2+i*iw,
                        pos[1]-h/2-mh,
                        pos[0]-w/2+(i+1)*iw,
                        pos[1]-h/2,
                        fill=fill, activefill=afill)
                    self.inputs[ob] = obj, i

                for i in range(0, b.output_count):
                    ow = w/b.output_count
                    ob = self.canvas.create_rectangle(
                        pos[0]-w/2+i*ow,
                        pos[1]+h/2,
                        pos[0]-w/2+(i+1)*ow,
                        pos[1]+h/2+mh,
                        fill=fill, activefill=afill)
                    self.outputs[ob] = obj, i

                bt = b.ident
                for p in o.params:
                    bt += " : " + p

                self.canvas.create_text(pos, font=FONTA,
                    text=bt, state=DISABLED)

            if o.kind == COMMENT:
                cid = obj
                comment = o
                h = 26
                w = 160
                pos = comment.pos()
                i = self.canvas.create_rectangle(
                    pos[0]-w/2, pos[1]-h/2, pos[0]+w/2, pos[1]+h/2,
                    fill="#ffff00", activefill="#ffffaa")
                self.objectsById[i] = cid, comment
                self.canvas.create_text(pos, font=FONTB,
                    text=comment.text, state=DISABLED, width=w)
