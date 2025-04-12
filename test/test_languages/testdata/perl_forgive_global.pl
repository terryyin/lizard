#!/usr/bin/perl

use strict;
use warnings;

# Global code with complexity
my $x = 1;
if ($x > 0) {
    print "Positive\n";
}
elsif ($x < 0) {
    print "Negative\n";
}
else {
    print "Zero\n";
}

# #lizard forgive global
# More global code with complexity
my $y = 2;
if ($y > 0) {
    print "Y is positive\n";
}
elsif ($y < 0) {
    print "Y is negative\n";
}

# This function should still be counted
sub test_function {
    my ($param) = @_;
    if ($param > 0) {
        print "Param is positive\n";
    }
    elsif ($param < 0) {
        print "Param is negative\n";
    }
    else {
        print "Param is zero\n";
    }
} 