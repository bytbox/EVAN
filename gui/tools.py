""" Tool list. See README for notes. """

from tkinter import *
from tkinter.simpledialog import *
import random

#!START local
from ftools import *
from ids import *
from program import *
#!END local

def newComment(prog, x, y):
    # TODO use the registry
    name = ''.join([random.choice('abcdefghijklmnopqrstuvwxyz') for i in range(12)])
    c = Comment("Hello, world")
    c._pos = (x, y)
    prog.objects[name] = c

class BlockTool:
    def __init__(self, name, p, i, o):
        self.name = name
        self.ins = i
        self.outs = o
        self.params = p

    def __call__(self, prog, x, y, root):
        name = ''.join([random.choice('abcdefghijklmnopqrstuvwxyz') for i in range(12)])
        if len(self.params) > 0:
            self.prompt(root)
        b = Block(self.name, self.ins, self.outs)
        b._pos = (x, y)
        prog.objects[name] = b

    def prompt(self, root):
        pd = ParamDialog(root)
        
class ParamDialog(SimpleDialog):
    def body(self, master):
        body = Frame(master)
        Label(body, text="Params").pack()
        

"""
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


"""

tools = {
    "Cancel": None,
    "Comment": newComment,
}

categories = {}

def addBlockTool(name, i, o):
    tools[name] = BlockTool(name, i, o)

def addBlockTools(toolInfo):
    for info in toolInfo:
        addBlockTool(info[0], info[1], info[2])

def add_category(name, toolinfo):
    ts = {}
    for info in toolinfo:
        ts[info[0]] = BlockTool(info[0], info[1], info[2], info[3])
    categories[name+">"] = ts
    #addBlockTools(toolinfo)

addFTools(add_category)
