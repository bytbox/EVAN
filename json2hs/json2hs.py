#!/usr/bin/env python3

import json
import sys

rendered = {}

def rid(i):
    """ Represent an EVAN identifier as a valid haskell identifier, returning
    the resulting string. """
    
    return "_"+i

def elem(i, n):
    """ Returns a string representing the haskell lambda expression to retreive
    element i of an n-tuple. """
    
    ps = ['_']*n
    ps[i] = 'x'
    return '(\ (' + ','.join(ps) + ') -> x)'

def render(p, i):
    if i in rendered:
        return
    rendered[i] = True
    o = p[i]
    line = "  "
    if o['kind'] == 'block':
        # First render each input
        for inp in o['inputs']:
            render(p, inp)
        if o['ident'] == 'Each':
            line += "" + rid(i) + " <- " + o['inputs'][0]
        elif o['ident'] == 'Done':
            line += "let " + rid(i) + " = "
            line += 'return (' + ','.join(o['inputs']) + ')'
        else:
            line += "let " + rid(i) + " = "
            line += rid(o['ident']) + ' (' + ','.join(o['inputs']) + ')'
    elif o['kind'] == 'pipe':
        # First render the source
        render(p, o['source'][0])
        line += "let " + rid(i) + " = "
        src = o['source']
        line += elem(src[1], p[src[0]]['output-count']) + ' ' + rid(src[0])
    print(line)

prog = json.load(sys.stdin)

imports = ['EVAN']

print ("module Main where")
for imp in imports:
    print ("import "+imp)

print ("main = do")
render(prog, 'Return1')
print ("  _Return1")
