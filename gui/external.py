""" Execution control. See README for notes. """

import os
import os.path
import shutil

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
    # Run evan-compile on the copied file

def run_prog(prog, ds):
    """ Runs the given program on the given dataset. """

    d = build_dir(prog)
