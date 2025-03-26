#!/usr/bin/perl

use strict;
use warnings;

package TernaryTest;

# Simple ternary operator
sub simple_ternary {
    my ($x) = @_;
    
    my $result = $x > 0 ? "positive" : "non-positive";
    
    return $result;
}

# Multiple ternary operators
sub multiple_ternary {
    my ($x) = @_;
    
    my $result = $x > 10 ? "large" : $x > 0 ? "positive" : "negative or zero";
    
    return $result;
}

# Ternary with complex conditions
sub complex_ternary {
    my ($x, $y) = @_;
    
    my $result = ($x > 0 && $y > 0) ? "both positive" : ($x < 0 && $y < 0) ? "both negative" : "mixed signs";
    
    return $result;
}

# Ternary in assignment
sub ternary_in_assignment {
    my ($data, $default) = @_;
    
    my $value = defined $data ? $data : $default;
    
    return $value;
}

# Ternary in return
sub ternary_in_return {
    my ($x) = @_;
    
    return $x > 0 ? $x : -$x;  # Return absolute value
}

# Ternary in function call
sub ternary_in_call {
    my ($x, $y) = @_;
    
    print $x > $y ? "X is larger\n" : "Y is larger or equal\n";
    
    return;
}

# Nested ternary operators in complex expression
sub nested_ternary {
    my ($x, $y, $z) = @_;
    
    my $max = $x > $y 
        ? ($x > $z ? $x : $z) 
        : ($y > $z ? $y : $z);
    
    return $max;
}

1; # End of package 