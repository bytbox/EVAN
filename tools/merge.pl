#!/usr/bin/perl

use strict;
use warnings;

use File::Basename qw/dirname/;

# This script is used to combine the various python files that compose the evan
# executable into a single script that can be copied, standalone (but for
# evan-compile etc.), to bin/.

sub write_script;

sub write_script {
	my ($fname, $dir) = @_;
	my ($local) = 0;
	my $fh;
	open $fh, "<$fname" or die "$fname: $!";
	while (my $line = <$fh>) {
		chomp $line;
		if ($line =~ /#!START local/) {
			$local = 1;
			next;
		} elsif ($line =~ /#!END local/) {
			$local = 0;
			next;
		}
		if ($local and $line =~ /from ([a-z]+) import \*/) {
			write_script "$dir/$1.py", $dir;
		} else {
			print "$line\n";
		}
		
	}
	close $fh;
}

print "usage: $0 script.py > output.py\n" and exit 1 unless $ARGV[0];
write_script $ARGV[0], dirname $ARGV[0];
