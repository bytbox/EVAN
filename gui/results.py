# Displaying results for the python+tk frontend. See README for notes.

import json
import re
from tkinter import *

def display_results(rs):
    """ Displays the given results. """

    r = json.loads(rs)
    root = Tk()
    root.title('Results')
    root.protocol('WM_DELETE_WINDOW', root.destroy)

    resFrame = Frame(root)
    resFrame.pack(side=LEFT, anchor='nw', fill=BOTH, expand=1)

    resDisp = Canvas(resFrame, bg='white', bd=2, relief=SUNKEN)
    resDisp.pack(side=LEFT, anchor='nw', fill=BOTH, expand=1)
    if isinstance(r, str):
       resDisp.create_text(20, 20, text=r)
    elif isinstance(r, dict):
        m = r['mime']
        k = r['kind']
        if 'data' in r:
            d = r['data']
        elif 'file' in r:
            fn = r['file']
            f = open(fn, "rb")
            d = f.read()
            f.close()
        else:
            # TODO ERROR
            return
        
        if re.match('^image/', m):
            img = PhotoImage(data=d)
            resDisp.create_image(30, 30, image=img)
        else:
            # TODO error
            return
    else:
        print(r)

