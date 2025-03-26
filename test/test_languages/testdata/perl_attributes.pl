#!/usr/bin/perl

use strict;
use warnings;

package TestAttributes;

# Method with a single attribute
sub method_with_attr : method {
    my ($self) = @_;
    return "I'm a method";
}

# Method with multiple attributes
sub lvalue_method : lvalue : method {
    my ($self) = @_;
    my $value = 42;
    $value;  # returns lvalue
}

# Method with attribute and conditions
sub complex_method : locked {
    my ($self, $x) = @_;
    
    if ($x > 0) {
        return "positive";
    }
    elsif ($x < 0) {
        return "negative";
    }
    else {
        return "zero";
    }
}

1; # End of package 