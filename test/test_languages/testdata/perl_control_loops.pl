#!/usr/bin/perl

use strict;
use warnings;

package LoopTest;

# Simple while loop
sub simple_while {
    my ($count) = @_;
    my $result = 0;
    
    while ($count > 0) {
        $result += $count;
        $count--;
    }
    
    return $result;
}

# While loop with condition
sub while_condition {
    my ($x, $max) = @_;
    my $result = 0;
    
    while ($x < $max && $result < 100) {
        $result += $x;
        $x++;
    }
    
    return $result;
}

# Nested while loops
sub nested_while {
    my ($rows, $cols) = @_;
    my $total = 0;
    my $i = 0;
    
    while ($i < $rows) {
        my $j = 0;
        while ($j < $cols) {
            $total += $i * $j;
            $j++;
        }
        $i++;
    }
    
    return $total;
}

# Simple until loop (opposite of while)
sub simple_until {
    my ($count) = @_;
    my $result = 0;
    
    until ($count <= 0) {
        $result += $count;
        $count--;
    }
    
    return $result;
}

# Until loop with complex condition
sub until_condition {
    my ($x, $max) = @_;
    my $result = 0;
    
    until ($x >= $max || $result >= 100) {
        $result += $x;
        $x++;
    }
    
    return $result;
}

# While with an if condition inside
sub while_with_if {
    my ($values) = @_;
    my $i = 0;
    my $sum_even = 0;
    my $sum_odd = 0;
    
    while ($i < scalar(@$values)) {
        if ($values->[$i] % 2 == 0) {
            $sum_even += $values->[$i];
        }
        else {
            $sum_odd += $values->[$i];
        }
        $i++;
    }
    
    return ($sum_even, $sum_odd);
}

1; # End of package 