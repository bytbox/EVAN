# Program data structures. See README for notes.

import json

class Graphical:
    """ The graphical representation of an object. This class doesn't actually
    interact with a canvas; it simply makes doing so in an efficient way
    possible. """

class Program:
    """ A program consists, in our model, of a set of blocks and pipes. """

    def __init__(self):
        self.blocks = {}
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
