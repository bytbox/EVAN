# Program data structures. See README for notes.

import json

class Graphical:
    """ The graphical representation of an object. This class doesn't actually
    interact with a canvas; it simply makes doing so in an efficient way
    possible. """

    def __init__(self):
        pass

    def asJSON(self):
        return json.dumps({})

class Program:
    """ A program consists, in our model, of a set of blocks and pipes. """

    def __init__(self):
        """ A program is not initialized blank - there is an input and an
        output block """

        self.blocks = {"Events": Block(), "Return": Block()}
        self.pipes = {}

    def asJSON(self):
        return json.dumps({})

class Block(Graphical):
    """ A block represents a function call. """

    def __init__(self):
        pass

    def asJSON(self):
        return json.dumps({})

class Pipe(Graphical):
    """ A pipe represents a variable. """

    def __init__(self):
        pass

    def asJSON(self):
        return json.dumps({})

def Program_fromJSON(j):
    return Program()

def Graphical_fromJSON(j):
    return Graphical()

def Block_fromJSON(j):
    return Block()

def Pipe_fromJSON(j):
    return Pipe()
