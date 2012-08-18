#!/usr/bin/perl

use strict;
use warnings;

use Data::Dumper;
use XML::Simple;

my ($fin, $fout) = (\*STDIN, \*STDOUT);

if (my $ifname = shift) {
	open $fin, '<', $ifname or die $!;
	if (my $ofname = shift) {
		open $fout, '>', $ofname or die $!;
	}
}

my $xml = new XML::Simple;
my $data = $xml->XMLin($fin);

print $fout <<END;
#include "foreign.h"
#include "interp.hh"
#include "util.hh"

#include <string>
#include <vector>
using namespace std;

simple_registry<Interpreter::Function> Interpreter::foreignFunctions({
END

my $IFUNC = "[] (vector <Param> ps, vector <Value> as) -> Value";
for my $catname (keys $data->{'category'}) {
	my $catcname = $data->{'category'}{$catname}{'cname'};
	my $fs = $data->{'category'}{$catname}{'foreign'};
	for my $fname (keys %$fs) {
		my $fcname = $fs->{$fname}{cname};
		print STDERR Dumper($fs->{$fname});
		print "\t{\"$fname\", ($IFUNC {\n";
		print "\t\treturn 0;\n";
		print "\t})},\n";
	}
}

print $fout <<END;
});
END

close $fin;
close $fout;

