""" Tool list. See README for notes. """

import random

#!START local
from ftools import *
from ids import *
from program import *
#!END local

# TODO properly generate ids.

def newComment(prog, x, y):
    # TODO make sure it's unique
    name = ''.join([random.choice('abcdefghijklmnopqrstuvwxyz') for i in range(12)])
    c = Comment("Hello, world")
    c._pos = (x, y)
    prog.objects[name] = c

class BlockTool:
    def __init__(self, name, i, o):
        self.name = name
        self.ins = i
        self.outs = o

    def __call__(self, prog, x, y):
        name = ''.join([random.choice('abcdefghijklmnopqrstuvwxyz') for i in range(12)])
        b = Block(self.name, self.ins, self.outs)
        b._pos = (x, y)
        prog.objects[name] = b

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
        ts[info[0]] = BlockTool(info[0], info[1], info[2])
    categories[name+">"] = ts
    #addBlockTools(toolinfo)

addFTools(add_category)
