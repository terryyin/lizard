#!/usr/bin/perl

use strict;
use warnings;

package CompoundConditionTest;

# Simple AND condition
sub simple_and {
    my ($x, $y) = @_;
    
    if ($x > 0 && $y > 0) {
        return $x * $y;
    }
    
    return 0;
}

# Simple OR condition
sub simple_or {
    my ($x, $y) = @_;
    
    if ($x > 0 || $y > 0) {
        return $x + $y;
    }
    
    return 0;
}

# Multiple AND conditions
sub multiple_and {
    my ($x, $y, $z) = @_;
    
    if ($x > 0 && $y > 0 && $z > 0) {
        return $x * $y * $z;
    }
    
    return 0;
}

# Multiple OR conditions
sub multiple_or {
    my ($x, $y, $z) = @_;
    
    if ($x < 0 || $y < 0 || $z < 0) {
        return -1;
    }
    
    return $x + $y + $z;
}

# Mixed AND/OR conditions
sub mixed_conditions {
    my ($x, $y, $z) = @_;
    
    if (($x > 0 && $y > 0) || $z > 10) {
        return $x + $y + $z;
    }
    
    return 0;
}

# Complex condition with parentheses
sub complex_with_parens {
    my ($x, $y, $z, $w) = @_;
    
    if (($x > $y && $y > $z) || ($z > $w && $w != 0)) {
        return 1;
    }
    
    return 0;
}

# Compound conditions in while loop
sub compound_in_loop {
    my ($max) = @_;
    my ($i, $j) = (0, 0);
    my $result = 0;
    
    while ($i < $max && $j < 10) {
        $result += $i;
        $i++;
        $j++;
    }
    
    return $result;
}

# Compound conditions with assignments
sub compound_with_assignment {
    my ($data) = @_;
    my $result = 0;
    
    my $item;
    while (($item = shift @$data) && $item != 0) {
        $result += $item;
    }
    
    return $result;
}

1; # End of package 