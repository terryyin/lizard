#!/usr/bin/perl

use strict;
use warnings;

package MyPackage;

# A method in a package
sub my_method {
    my ($self, $param1) = @_;
    
    if ($param1 > 0) {
        print "Positive\n";
    }
    elsif ($param1 < 0) {
        print "Negative\n";
    }
    
    return $param1 * 2;
}

# Another method to test multiple methods in package
sub another_method {
    my ($self) = @_;
    return 42;
}

1; # End of package 