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
my $pdir = $ARGV[0] || "$ed/../evanlib/EVAN";
my @hsfiles = glob "$pdir/*.lhs";
for my $f (@hsfiles) {
	$f =~ /\/(\w+)\.lhs/;
	print FOUT "    add_category('$1', [\n";
	open FIN, $f or die $!;
	my $hs = "";
	$hs .= $_ while <FIN>;
	close FIN or die $!;
	while ($hs =~ /^>? *_([A-Z]\w*) *:: *(.*=> *)?(.*?) *-> (.*?) *-> *(.*)/mg) {
		my ($name, $paramstr, $argstr, $restr) = ($1, $3, $4, $5);
		my @params = ptup $paramstr;
		my @args = ptup $argstr;
		my @res = ptup $restr;
		my ($pl, $al, $rl) = ($#params+1, $#args+1, $#res+1);
		my $pstr = join ",", map {"'$_'"} @params;
		print FOUT "        ('$name', [$pstr], $al, $rl),\n";
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
