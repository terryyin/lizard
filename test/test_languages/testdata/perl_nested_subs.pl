#!/usr/bin/perl

use strict;
use warnings;

package NestedSubTest;

# Regular function with nested subroutine
sub outer_function {
    my ($param) = @_;
    my $result = 0;
    
    # Nested subroutine defined inside another
    sub nested_sub {
        my ($x) = @_;
        return $x * 2;
    }
    
    $result = nested_sub($param);
    return $result;
}

# Function with multiple nested subroutines
sub multi_nested {
    my ($x, $y) = @_;
    
    # First nested sub
    sub nested_one {
        my ($a) = @_;
        return $a + 10;
    }
    
    # Second nested sub
    sub nested_two {
        my ($b) = @_;
        return $b * 5;
    }
    
    return nested_one($x) + nested_two($y);
}

# Function with nested subroutine that has conditions
sub outer_with_complex_nested {
    my ($value) = @_;
    
    sub complex_nested {
        my ($n) = @_;
        if ($n > 10) {
            return $n * 2;
        } elsif ($n > 5) {
            return $n + 10;
        } else {
            return $n;
        }
    }
    
    return complex_nested($value);
}

# Function with lexical (anonymous) nested subroutine
sub with_lexical_sub {
    my ($param) = @_;
    my $result = 0;
    
    # This is a lexical sub that won't be visible outside
    my $lexical_sub = sub {
        my ($x) = @_;
        return $x * 3;
    };
    
    $result = $lexical_sub->($param);
    return $result;
}

# Function with nested anonymous sub in a block
sub with_anon_in_block {
    my ($param) = @_;
    
    if ($param > 0) {
        my $helper = sub {
            my ($x) = @_;
            return $x * $x;
        };
        
        return $helper->($param);
    }
    
    return 0;
}

1; # End of package 