#!/usr/bin/perl

use strict;
use warnings;

# #lizard forgives
sub complex_sub_to_forgive {
    my ($param1, $param2, $param3) = @_;
    
    if ($param1 > 0) {
        print "Positive\n";
    }
    elsif ($param2 > 0) {
        print "Second param is positive\n";
    }
    else {
        if ($param3 > 0) {
            print "Third param is positive\n";
        }
        else {
            print "All negative or zero\n";
        }
    }
    
    return $param1 + $param2 + $param3;
} 