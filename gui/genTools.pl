#!/usr/bin/perl

use File::Basename qw/dirname/;

use strict;
use warnings;

sub ptup;

# TODO support aliases

open FOUT, ">ftools.py" or die $!;

print FOUT <<END;
""" Auto-generated file. Do not modify. """

def addFTools(add_category):
END

my $ed = dirname $0;
my $pdir = $ARGV[0] || "$ed/../lib/EVAN";
my @hsfiles = glob "$pdir/*.lhs";
for my $f (@hsfiles) {
	$f =~ /\/(\w+)\.lhs/;
	print FOUT "    add_category('$1', [\n";
	open FIN, $f or die $!;
	my $hs = "";
	$hs .= $_ while <FIN>;
	close FIN or die $!;
	while ($hs =~ /^> *_([A-Z]\w*) *:: *(.*=> *)?(.*?) *-> *(.*)/mg) {
		my ($name, $argstr, $restr) = ($1, $3, $4);
		my @args = ptup $argstr;
		my @res = ptup $restr;
		my ($al, $rl) = ($#args+1, $#res+1);
		print FOUT "        ('$name', $al, $rl),\n";
	}
	print FOUT "    ])\n";
}

sub ptup {
	my $as = shift;
	$as =~ s/^\( *//;
	$as =~ s/ *\)$//;
	my @args = split /,/, $as;
	return @args;
}

close FOUT;
