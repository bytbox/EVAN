#!/usr/bin/perl

use strict;
use warnings;

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

use Data::Dumper;

for my $cname (keys $data->{'category'}) {
	print $fout "$cname\n";
	my $cat = $data->{'category'}{$cname}{'builtin'};
	print $fout Dumper($cat);
}

close $fin;
close $fout;

