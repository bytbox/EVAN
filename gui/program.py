# Program data structures. See README for notes.

import json

class Program:
    """ A program consists, in our model, of a set of blocks and pipes. """

    def __init__(self):
        self.blocks = {}
        self.pipes = {}

    def asJSON(self):
        return json.dumps({})

class Block:
    """ A block represents a function call. """

    def __init__(self):
        pass

    def asJSON(self):
        return json.dumps({})

class Pipe:
    """ A pipe represents a variable. """

    def __init__(self):
        pass

    def asJSON(self):
        return json.dumps({})
