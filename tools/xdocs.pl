#!/usr/bin/perl

use strict;
use warnings;

use JSON;

# This script is used to extract and JSON-encode documentation from the EVAN
# libraries.

my $ldn = shift;
print "usage: $0 <dirname>\n" and exit 1 unless $ldn;

my $categories = {};

opendir DH, $ldn or die $!;
while (readdir DH) {
	next if /^\./;
	next unless /\.lhs$/;
	s/\.lhs$//;
	$categories->{$_}=1;
}
closedir DH or die $!;

my $time = localtime time;
my $docs = {
	generated => $time,
	categories => $categories,
};

my $json = JSON->new->utf8->pretty;
print $json->encode($docs);
