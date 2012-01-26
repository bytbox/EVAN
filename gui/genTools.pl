#!/usr/bin/perl

use File::Basename qw/dirname/;

use strict;
use warnings;

my $ed = dirname $0;
my $pdir = $ARGV[0] || "$ed/../lib/EVAN";
my @hsfiles = glob "$pdir/*.lhs";
for my $f (@hsfiles) {
	$f =~ /\/(\w+)\.lhs/;
	print "== $1\n";
	open FIN, $f or die $!;
	my $hs = "";
	$hs .= $_ while <FIN>;
	close FIN or die $!;
	while ($hs =~ /^> *_([A-Z]\w*) *:: *(.*=> *)?(.*?) *-> *(.*)/mg) {
		my ($name, $args, $res) = ($1, $3, $4);
		print "$name :: $args -> $res\n";
	}
}
