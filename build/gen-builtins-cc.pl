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

print $fout <<END;
#include "builtins.hh"

#include <string>
#include <vector>
using namespace std;

const vector <Category> BuiltinInfo::categories =
{
END

for my $cname (keys $data->{'category'}) {
	my $cat = $data->{'category'}{$cname}{'builtin'};
	print $fout "\tCategory(\"$cname\", {\n";
	for my $bname (keys $cat) {
		my $builtin = $cat->{$bname};
		my $desc = $builtin->{description};
		$desc = "" if ref($desc);
		$desc =~ s/\n/ /msg;
		$desc =~ s/^\s+//;
		$desc =~ s/\s+$//;
		$desc =~ s/\s+/ /msg;
		$desc =~ s/"/\\"/msg; # TODO escape better
		print $fout "\t\tBuiltin(\"$bname\",\"$desc\"),\n";
	}
	print $fout "\t}),\n";
}

print $fout <<END;
};
END

close $fin;
close $fout;

