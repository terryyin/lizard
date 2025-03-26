#!/usr/bin/perl

use strict;
use warnings;
use feature 'switch';  # Required for given-when

package GivenWhenTest;

# Simple given-when with literal values
sub simple_given_when {
    my ($value) = @_;
    
    given ($value) {
        when (1) { return "One"; }
        when (2) { return "Two"; }
        when (3) { return "Three"; }
        default  { return "Unknown"; }
    }
}

# Given-when with smart matching
sub given_when_smart_match {
    my ($fruit) = @_;
    
    given ($fruit) {
        when (/^a/) { return "Starts with A"; }
        when (/^b/) { return "Starts with B"; }
        when ([qw(orange peach plum)]) { return "Orange or peach or plum"; }
        default { return "Other fruit"; }
    }
}

# Given-when with nested logic
sub given_when_with_nested {
    my ($code, $value) = @_;
    
    given ($code) {
        when (1) {
            given ($value) {
                when ($_ > 10) { return "Code 1, value > 10"; }
                when ($_ > 0)  { return "Code 1, value > 0"; }
                default        { return "Code 1, value <= 0"; }
            }
        }
        when (2) {
            if ($value > 0) {
                return "Code 2, positive value";
            } else {
                return "Code 2, non-positive value";
            }
        }
        default {
            return "Unknown code";
        }
    }
}

# Given-when with continue
sub given_when_with_continue {
    my ($points) = @_;
    my $result = "";
    
    given ($points) {
        when ($_ > 100) { $result .= "High score! "; continue; }
        when ($_ > 50)  { $result .= "Good score! "; continue; }
        when ($_ > 20)  { $result .= "Fair score! "; continue; }
        default         { $result .= "You participated! "; }
    }
    
    return $result;
}

# Given-when with complex conditions
sub given_when_complex {
    my ($data) = @_;
    
    given ($data) {
        when (!defined $_) { return "undefined"; }
        when ($_->isa('HASH')) { return "hash reference"; }
        when ($_->isa('ARRAY')) { return "array reference"; }
        when (ref eq 'CODE') { return "code reference"; }
        when (length > 10) { return "long string"; }
        default { return "something else"; }
    }
}

1; # End of package 