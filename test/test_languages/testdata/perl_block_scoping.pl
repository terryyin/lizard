#!/usr/bin/perl

use strict;
use warnings;

package BlockScopingTest;

# Function with block-level variable scoping
sub with_block_scope {
    my $outer = 10;
    
    {
        # Block-scoped variable
        my $inner = 20;
        $outer += $inner;
    }
    
    return $outer;
}

# Function with multiple nested blocks
sub multi_block_scope {
    my $result = 0;
    
    {
        my $a = 5;
        $result += $a;
        
        {
            my $b = 10;
            $result += $b;
            
            {
                my $c = 15;
                $result += $c;
            }
        }
    }
    
    return $result;
}

# Function with block and conditional
sub block_with_condition {
    my ($x) = @_;
    my $result = 0;
    
    {
        my $temp = $x * 2;
        
        if ($temp > 10) {
            $result = $temp;
        } else {
            $result = 10;
        }
    }
    
    return $result;
}

# Function with loop in block
sub block_with_loop {
    my ($max) = @_;
    my $result = 0;
    
    {
        my $counter = 0;
        
        while ($counter < $max) {
            $result += $counter;
            $counter++;
        }
    }
    
    return $result;
}

# Function with C-style for loop in block
sub block_with_for {
    my ($max) = @_;
    my $result = 0;
    
    {
        my $i;
        for ($i = 0; $i < $max; $i++) {
            $result += $i;
        }
    }
    
    return $result;
}

1; # End of package 