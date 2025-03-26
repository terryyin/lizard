#!/usr/bin/perl

use strict;
use warnings;

# A simple Perl subroutine with complexity
sub sub_with_complexity {
    my ($param1, $param2) = @_;
    
    if ($param1 > 0) {
        print "Positive\n";
    }
    
    return $param1 + $param2;
} 