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
	my $fs = $data->{'category'}{$catname}{'foreign'};
	print $fout "\tForeignCategory(\"$catname\", \"$catcname\", {\n";
	for my $fname (keys %$fs) {
		my $fcname = $fs->{$fname}{cname};
		my $desc = $fs->{$fname}{description};
		$desc = "" if ref($desc);
		$desc =~ s/\n/ /msg;
		$desc =~ s/^\s+//;
		$desc =~ s/\s+$//;
		$desc =~ s/\s+/ /msg;
		$desc =~ s/"/\\"/msg; # TODO escape better

		print $fout "\t\tForeignFunc(\"$fname\", \"$fcname\", \"\", BlockType({}, {}, {})),\n";
	}
	print $fout "\t}),\n";
}

print $fout <<END;
};
END

close $fin;
close $fout;

