#!/usr/bin/perl

use strict;
use warnings;

use JSON;

# This script is used to extract and JSON-encode reference documentation from
# the EVAN libraries.

my $ldn = shift;
print "usage: $0 <dirname>\n" and exit 1 unless $ldn;

my $categories = {};

opendir DH, $ldn or die $!;
while (readdir DH) {
	next if /^\./;
	next unless /\.lhs$/;
	my ($fn, $cn) = ("$ldn/$_", $_);
	$cn =~ s/\.lhs$//;
	my $ci = {
		title => $cn,
	};
	my $cl = [];
	open FIN, "$fn" or die $!;
	while (my $l = <FIN>) {
		next unless $l =~ /^!/;
		$l =~ /^! ([A-Z]\w*) (.*)/;
		my ($fn, $sig) = ($1, $2);
		my $t = "";
		while (my $l = <FIN>) {
			last if $l =~ />/;
			$t .= $l;
		}
		my $fi = {
			name => $fn,
			sig => $sig,
			docs => $t,
		};
		push @$cl, $fi;
	}
	close FIN or die $!;
	$ci->{functions} = $cl;
	$categories->{$cn}=$ci;
}
closedir DH or die $!;

my $time = localtime time;
my $docs = {
	generated => $time,
	categories => $categories,
};

my $json = JSON->new->utf8->pretty;
print $json->encode($docs);
