#!/usr/bin/perl

use strict;
use warnings;

package ControlTest;

# Simple if statement
sub simple_if {
    my ($x) = @_;
    
    if ($x > 0) {
        return "positive";
    }
    
    return "non-positive";
}

# If-else statement
sub if_else {
    my ($x) = @_;
    
    if ($x > 0) {
        return "positive";
    }
    else {
        return "non-positive";
    }
}

# If-elsif-else chain
sub if_elsif_else {
    my ($x) = @_;
    
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

# Multiple elsif blocks
sub multi_elsif {
    my ($grade) = @_;
    
    if ($grade >= 90) {
        return "A";
    }
    elsif ($grade >= 80) {
        return "B";
    }
    elsif ($grade >= 70) {
        return "C";
    }
    elsif ($grade >= 60) {
        return "D";
    }
    else {
        return "F";
    }
}

# Nested if statements
sub nested_if {
    my ($x, $y) = @_;
    
    if ($x > 0) {
        if ($y > 0) {
            return "both positive";
        }
        else {
            return "only x positive";
        }
    }
    else {
        if ($y > 0) {
            return "only y positive";
        }
        else {
            return "both non-positive";
        }
    }
}

1; # End of package 