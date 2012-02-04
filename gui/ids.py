""" Id registry. See README for notes. """

#!START local

#!END local

class Registry:
    def __init__(self, n=1):
        self.n = n

    def next(self):
        n = self.n
        self.n = self.n+1
        return str(n)

    def named(self, name):
        return "_" + name + "_reg_" + self.next()

    def as_object(self):
        return {
            "kind": 'registry',
            "n": self.n
        }

