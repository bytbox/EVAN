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
		my @args;
		my $type = $fs->{$fname}{type};
		my $parameters = $type->{parameter} || [];
		$parameters = [$parameters] if (ref $parameters eq 'HASH');
		my $pn = 0;
		for my $p (@$parameters) {
			die "parameters must be builtins" unless $p->{builtin};
			push @args, "ps[$pn]";
			$pn++;
		}

		my $arguments = $type->{argument} || [];
		$arguments = [$arguments] if (ref $arguments eq 'HASH');
		my $an = 0;
		for my $p (@$arguments) {
			push @args, "as[$an]";
			$an++;
		}

		my $argstr = join ',', @args;
		print "\t{\"$fname\", ($IFUNC {\n";
		print "\t\treturn $fcname($argstr);\n";
		print "\t})},\n";
	}
}

print $fout <<END;
});
END

close $fin;
close $fout;

