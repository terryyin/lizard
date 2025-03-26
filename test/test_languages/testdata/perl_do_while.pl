#!/usr/bin/perl

use strict;
use warnings;

package DoWhileTest;

# Simple do-while loop
sub simple_do_while {
    my ($max) = @_;
    my $counter = 0;
    my $sum = 0;
    
    do {
        $sum += $counter;
        $counter++;
    } while ($counter < $max);
    
    return $sum;
}

# Do-while with complex condition
sub complex_do_while {
    my ($x, $y) = @_;
    my $result = 0;
    
    do {
        $result += $x;
        $x--;
    } while ($x > 0 && $y-- > 0);
    
    return $result;
}

# Nested do-while loops
sub nested_do_while {
    my ($max) = @_;
    my ($i, $j) = (0, 0);
    my $result = 0;
    
    do {
        $j = 0;
        do {
            $result += $i * $j;
            $j++;
        } while ($j < 3);
        $i++;
    } while ($i < $max);
    
    return $result;
}

# Do-while with if condition inside
sub do_while_with_if {
    my ($max) = @_;
    my $i = 0;
    my $sum = 0;
    
    do {
        if ($i % 2 == 0) {
            $sum += $i;  # Add even numbers
        }
        $i++;
    } while ($i < $max);
    
    return $sum;
}

# Do-while with control flow statements
sub do_while_with_control {
    my ($max) = @_;
    my $i = 0;
    my $result = 0;
    
    do {
        $i++;
        next if $i % 3 == 0;  # Skip multiples of 3
        $result += $i;
        last if $result > $max;  # Stop if sum exceeds max
    } while ($i < 20);
    
    return $result;
}

1; # End of package 