#!/usr/bin/perl

use strict;
use warnings;

package UnlessTest;

# Simple unless statement
sub simple_unless {
    my ($x) = @_;
    
    unless ($x > 0) {
        return "non-positive";
    }
    
    return "positive";
}

# Unless-else statement
sub unless_else {
    my ($x) = @_;
    
    unless ($x > 0) {
        return "non-positive";
    }
    else {
        return "positive";
    }
}

# Unless with complex condition
sub unless_complex {
    my ($x, $y) = @_;
    
    unless ($x > 0 && $y > 0) {
        return "at least one non-positive";
    }
    
    return "both positive";
}

# Nested unless statements
sub nested_unless {
    my ($x, $y) = @_;
    
    unless ($x < 0) {
        unless ($y < 0) {
            return "both non-negative";
        }
        
        return "x non-negative, y negative";
    }
    
    return "x negative";
}

# Unless with multiple statements
sub unless_multi {
    my ($x) = @_;
    
    unless ($x > 10) {
        $x = $x * 2;
        
        if ($x > 10) {
            return "doubled and now greater than 10";
        }
        
        return "still small even after doubling";
    }
    
    return "already greater than 10";
}

1; # End of package 