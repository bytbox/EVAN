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

		my $args = $builtin->{type}{argument} || [];
		$args = [$args] if (ref $args eq 'HASH');
		my $arg_str = "";
		for my $a (@$args) {
			$arg_str .= "&anyType,";
		}

		my $params = $builtin->{type}{parameter} || [];
		$params = [$params] if (ref $params eq 'HASH');
		my $param_str = "";
		for my $a (@$params) {
			$param_str .= "&anyType,";
		}

		print $fout <<END;
		Builtin(
			\"$bname\",
			\"$desc\",
			BlockType(
				std::vector<Type *>{$param_str},
				std::vector<Type *>{$arg_str},
				{}
			)
		),
END
	}
	print $fout "\t}),\n";
}

print $fout <<END;
};
END

close $fin;
close $fout;

