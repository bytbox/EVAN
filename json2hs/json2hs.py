#!/usr/bin/env python3

import json
import sys

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
    o = p[i]
    r = ""
    deps = [] # the dependency stack
    k = o['kind']
    if k == 'block':
        ident = o['ident']
        if ident == 'Done':
            rs, deps = render(p, o['inputs'][0])
            d = deps.pop()
            rds, ds = render(p, d)
            r = rds + rid(i) + ' = ' + rs + '} ; return ' + rid(o['inputs'][0]) + '} ; '
        elif ident == 'Each':
            # We don't render the dependency here, we just pass it down to be
            # handled by 'Done'.
            inp = o['inputs'][0]
            deps.append(inp)
            r = "do {" + rid(i) + ' <- ' + rid(inp) + ' ; let {'
        else:
            ds = ""
            r = rid(i) + ' = '
            for inp in o['inputs']:
                d, dep = render(p, inp)
                deps += dep
                ds += d
            r += rid(ident) + ' ('
            r += ','.join(['_'+x for x in o['inputs']]) + ') ; '
            r = ds + r
    elif k == 'pipe':
        src = o['source']
        rp, deps = render(p, src[0])
        r = rp + rid(i) + " = " + elem(src[1], p[src[0]]['output-count']) + ' '
        r += rid(src[0]) + ';'
    return r, deps

prog = json.load(sys.stdin)

imports = ['EVAN']

print ("module Main where")
for imp in imports:
    print ("import "+imp)

p, deps = render(prog, 'Return1')
print ("main = do { let {" + p + "}; _Return1 }")
