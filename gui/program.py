""" Program data structures. See README for notes. """

import json

# The distance to shift when adding a block
POS_SHIFT = 20

last_x = 0
last_y = 0

class Json:
    def as_json(self):
        return json.dumps(self.as_object())

class Graphical:
    """ The graphical representation of an object. This class doesn't actually
    interact with a canvas; it simply makes doing so in an efficient way
    possible. """

    def __init__(self):
        global last_x, last_y
        x=last_x+POS_SHIFT*3
        y=last_y+POS_SHIFT
        last_x, last_y = x, y
        self.pos = x, y
        self.ids = []

    def as_object(self):
        """ Convert to a json-able object. """

        return {}

class Program(Json):
    """ A program consists, in our model, of a set of blocks and pipes. """

    def __init__(self):
        """ A program is not initialized blank - there is an input and an
        output block """

        Json.__init__(self)
        self.blocks = {"Events": Block(), "Return": Block()}
        self.pipes = {}

    def as_object(self):
        """ Convert to a json-able object. """

        top = {}

        # add all blocks
        for block in self.blocks:
            top[block] = self.blocks[block].as_object()

        # add all pipes
        for pipe in self.pipes:
            top[pipe] = self.pipes[pipe].as_object()

        return top

class Block(Graphical):
    """ A block represents a function call. """

    def __init__(self):
        Graphical.__init__(self)

    def as_object(self):
        """ Convert to a json-able object. """

        return {}

class Pipe(Graphical):
    """ A pipe represents a variable. """

    def __init__(self):
        pass

    def as_object(self):
        """ Convert to a json-able object. """

        return {}

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
