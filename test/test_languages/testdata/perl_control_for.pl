#!/usr/bin/perl

use strict;
use warnings;

package ForLoopTest;

# Traditional C-style for loop
sub c_style_for {
    my ($max) = @_;
    my $sum = 0;
    
    for (my $i = 0; $i < $max; $i++) {
        $sum += $i;
    }
    
    return $sum;
}

# For loop with complex condition
sub for_complex_condition {
    my ($max) = @_;
    my $sum = 0;
    
    for (my $i = 0; $i < $max && $sum < 100; $i++) {
        $sum += $i;
    }
    
    return $sum;
}

# Nested for loops
sub nested_for {
    my ($rows, $cols) = @_;
    my @matrix;
    
    for (my $i = 0; $i < $rows; $i++) {
        for (my $j = 0; $j < $cols; $j++) {
            $matrix[$i][$j] = $i * $j;
        }
    }
    
    return \@matrix;
}

# Simple foreach loop over array
sub simple_foreach_array {
    my @numbers = (1, 2, 3, 4, 5);
    my $sum = 0;
    
    foreach my $num (@numbers) {
        $sum += $num;
    }
    
    return $sum;
}

# Foreach with implicit variable $_
sub foreach_implicit {
    my @numbers = (1, 2, 3, 4, 5);
    my $sum = 0;
    
    foreach (@numbers) {
        $sum += $_;
    }
    
    return $sum;
}

# Foreach with hash
sub foreach_hash {
    my %scores = (
        'Alice' => 95,
        'Bob' => 85,
        'Charlie' => 75
    );
    my $total = 0;
    
    foreach my $student (keys %scores) {
        $total += $scores{$student};
    }
    
    return $total;
}

# For loop with control flow (next/last)
sub for_with_control_flow {
    my ($max) = @_;
    my $sum = 0;
    
    for (my $i = 0; $i < $max; $i++) {
        next if $i % 2 == 0;  # Skip even numbers
        last if $i > 10;      # Stop if i > 10
        $sum += $i;
    }
    
    return $sum;
}

1; # End of package 