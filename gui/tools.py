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
        pinf = []
        if len(self.params) > 0:
            pinf = self.prompt(root)
        b = Block(self.name, self.ins, self.outs, params=pinf)
        b._pos = (x, y)
        prog.objects[name] = b

    def prompt(self, root):
        pd = ParamDialog(root, self.name, self.params)
        return pd.res
        
class ParamDialog(Dialog):
    
    def __init__(self, parent, name, ps):
        self.name = name
        self.ps = ps
        self.es = [None]*len(self.ps)
        self.res = [None]*len(self.ps)
        Dialog.__init__(self, parent, "Block Parameters")

    def body(self, master):
        i = 0
        for par in self.ps:
            Label(master, text=par).grid(row=i)
            self.es[i] = Entry(master)
            self.es[i].grid(row=i, column=1)
            i += 1
        return self.es[0]

    def apply(self):
        i = 0
        for par in self.ps:
            r = self.es[i].get()
            self.res[i] = r
            i += 1
        
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
