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
#include "foreigns.hh"

#include <string>
#include <vector>
using namespace std;

const vector<ForeignCategory> ForeignInfo::categories =
{
END

for my $catname (keys $data->{'category'}) {
	my $catcname = $data->{'category'}{$catname}{'cname'};
	print $fout "\tForeignCategory(\"$catname\", \"$catcname\", {}),\n";
}

print $fout <<END;
};
END

close $fin;
close $fout;

