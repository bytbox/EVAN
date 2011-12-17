# Program data structures. See README for notes.

import json

class Graphical:
    """ The graphical representation of an object. This class doesn't actually
    interact with a canvas; it simply makes doing so in an efficient way
    possible. """

    def __init__(self):
        pass

    def as_json(self):
        return json.dumps({})

class Program:
    """ A program consists, in our model, of a set of blocks and pipes. """

    def __init__(self):
        """ A program is not initialized blank - there is an input and an
        output block """

        self.blocks = {"Events": Block(), "Return": Block()}
        self.pipes = {}

    def as_json(self):
        return json.dumps({})

class Block(Graphical):
    """ A block represents a function call. """

    def __init__(self):
        pass

    def as_json(self):
        return json.dumps({})

class Pipe(Graphical):
    """ A pipe represents a variable. """

    def __init__(self):
        pass

    def as_json(self):
        return json.dumps({})

def program_from_json(j):
    """ Create a program object from the given JSON string. """
    return Program()

def graphical_from_json(j):
    """ Create a graphics description object from the given JSON string. """
    return Graphical()

def block_from_json(j):
    """ Create a block from the given JSON string. """
    return Block()

def pipe_from_json(j):
    """ Create a pipe from the given JSON string. """
    return Pipe()
