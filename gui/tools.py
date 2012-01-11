import random

#!START local
from program import *
#!END local

def newComment(prog, x, y):
    # TODO make sure it's unique
    name = ''.join([random.choice('abcdefghijklmnopqrstuvwxyz') for i in range(12)])
    c = Comment("Hello, world")
    c._pos = (x, y)
    prog.objects[name] = c

def newCount(prog, x, y):
    name = ''.join([random.choice('abcdefghijklmnopqrstuvwxyz') for i in range(12)])
    b = Block("Count", 1, 1)
    b._pos = (x, y)
    prog.objects[name] = b 

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
    "Count": BlockTool("Count", 1, 1),
}

