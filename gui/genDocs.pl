#!/usr/bin/perl

use strict;
use warnings;

open FIN, "../docs/reference.json" or die $!;
open FOUT, ">fdocs.py" or die $!;
print FOUT <<END;
""" Auto-generated file. Do not modify. """

docs = """
END
while (<FIN>) {
	print FOUT;
}
print FOUT <<END;
"""
END
close FOUT or die $!;
close FIN or die $!;
