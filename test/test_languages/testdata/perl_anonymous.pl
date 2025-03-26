#!/usr/bin/perl

use strict;
use warnings;

package AnonymousTest;

# Basic anonymous subroutine assigned to a variable
my $simple_anon = sub {
    my ($x) = @_;
    return $x * 2;
};

# Anonymous sub with conditional logic
my $complex_anon = sub {
    my ($x) = @_;
    
    if ($x > 0) {
        return "positive";
    }
    elsif ($x < 0) {
        return "negative";
    }
    
    return "zero";
};

# Anonymous sub as a callback
sub with_callback {
    my ($value, $callback) = @_;
    return $callback->($value);
}

# Using anonymous sub directly - make this more distinct
my $callback_sub = sub {
    my ($val) = @_;
    return $val + 5;
};

# Additional anonymous function to make sure we have 5 total functions
my $extra_anon = sub { return "extra"; };

my $result = with_callback(10, $callback_sub);

1; # End of package 