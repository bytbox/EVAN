""" Execution control. See README for notes. """

import os
import os.path
import shutil
import subprocess

BUILD_DIR = "_evan_build"

def build_dir(prog):
    """ Creates, if necessary, a directory to be used for builds, and returns
    the name. """

    fn = prog.fname
    dn = os.path.dirname(fn)
    bdn = os.path.join(dn, BUILD_DIR)
    if not os.path.exists(bdn):
        os.mkdir(bdn)
    sbdn = os.path.join(bdn, os.path.basename(fn))
    if not os.path.exists(sbdn):
        os.mkdir(sbdn)
    return sbdn

def compile_prog(prog):
    """ Compiles the given program. """

    d = build_dir(prog)
    # Copy the .evan file into the build dir
    shutil.copy(prog.fname, d)
    fname = os.path.join(d, prog.fname)
    # Run evan-compile on the copied file
    PIPE=subprocess.PIPE
    p = subprocess.Popen(["evan-compile", fname], stdout=PIPE, stderr=PIPE)
    out, err = p.communicate()
    e = err.decode('utf-8')
    if len(e) > 0:
        handle_error(e)
        return False
    return True

def run_prog(prog, datafname):
    """ Runs the given program on the given dataset. The results are returned.
    """

    d = build_dir(prog)
    xname = os.path.join(d, os.path.splitext(prog.fname)[0])
    PIPE = subprocess.PIPE
    p = subprocess.Popen([xname, datafname], stdout=PIPE, stderr=PIPE)
    out, err = p.communicate()
    e = err.decode('utf-8')
    if len(e) > 0:
        handle_error(e)
        return None

    return out.decode('utf-8')

def handle_error(e):
    print("ERR: " + e) # TODO dialog or error pane or something

