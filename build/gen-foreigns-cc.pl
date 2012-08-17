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

