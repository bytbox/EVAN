""" Program data structures. See README for notes. """

import json

# The distance to shift when adding a block
POS_SHIFT = 0, 60

last = 100, 0

COMMENT = "comment"
PIPE = "pipe"
BLOCK = "block"

class Json:
    def as_json(self):
        return json.dumps(self.as_object(), indent=2)

class Graphical:
    """ The graphical representation of an object. This class doesn't actually
    interact with a canvas; it simply makes doing so in an efficient way
    possible. """

    def init_pos(self):
        global last
        pos = (last[0] + POS_SHIFT[0], last[1] + POS_SHIFT[1])
        last = pos
        self._pos = pos
        self.ids = []

    def pos(self):
        return self._pos

    def move(self, dx, dy):
        """ Move this element by the specified amount. """

        self._pos = ((self._pos[0]+dx), (self._pos[1]+dy))

    def g_as_object(self):
        """ Convert to a json-able object. """

        return {"x": self._pos[0], "y": self._pos[1]}

class Program(Json):
    """ A program consists, in our model, of a set of blocks and pipes. """

    def __init__(self):
        """ A program is not initialized blank - there is an input and an
        output block """

        Json.__init__(self)
        self.blocks = {
            "Events1": Block("Events", 0, 1),
            "Return1": Block("Return", 1, 0),
        }
        self.pipes = {}
        self.comments = {"_comment1": Comment("Hello, world!")}

    def add_pipe(self, p):
        """ Add a pseudo-anonymous pipe. """

        name = "_pipe_"+str(len(self.pipes))
        self.pipes[name] = p
        d, i = p.dest
        self.blocks[d].inputs[i] = name

    def as_object(self):
        """ Convert to a json-able object. """

        top = {}

        # add all blocks
        for block in self.blocks:
            top[block] = self.blocks[block].as_object()

        # add all pipes
        for pipe in self.pipes:
            top[pipe] = self.pipes[pipe].as_object()

        # add all comments
        for c in self.comments:
            top[c] = self.comments[c].as_object()

        return top

class Comment(Json, Graphical):
    """ A comment. """

    def __init__(self, text):
        Json.__init__(self)
        Graphical.init_pos(self)
        self.text = text

    def as_object(self):
        """ Convert to a json-able object."""

        return {
            "kind": COMMENT,
            "graphics": self.g_as_object(),
            "text": self.text
        }

class Block(Json, Graphical):
    """ A block represents a function call. """

    def __init__(self, i, ic=0, oc=0):
        Json.__init__(self)
        Graphical.init_pos(self)
        self.ident = i
        self.input_count = ic
        self.output_count = oc

        # initialize input array
        self.inputs = [None]*ic

    def as_object(self):
        """ Convert to a json-able object. """

        return {
            "kind": BLOCK,
            "ident": self.ident,
            "output-count": self.output_count,
            "input-count": self.input_count,
            "inputs": self.inputs,
            "graphics": self.g_as_object(),
        }

class Pipe(Json):
    """ A pipe represents a variable. """

    def __init__(self, source, dest, i=None):
        Json.__init__(self)
       
        if i is None:
            i = "_pipe_" + source[0] + "__" + str(source[1])
        self.ident = i
        self.source = source
        self.dest = dest

    def as_object(self):
        """ Convert to a json-able object. """

        return {
            "kind": PIPE,
            "ident": self.ident,
            "source": self.source,
            "destination": self.dest,
        }

def program_from_json(j):
    """ Create a program object from the JSON string in the given file. """

    return Program()
