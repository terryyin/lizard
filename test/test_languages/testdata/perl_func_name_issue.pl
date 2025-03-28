#!/usr/bin/perl

use strict;
use warnings;

# Simple function declaration
sub fetch {
    return "Basic fetch";
}

# Function with empty parameter list
sub fetch() {
    return "Fetch with empty params";
}

# Function with single parameter
sub fetch($) {
    my ($param) = @_;
    return "Fetch with single param: $param";
}

# Function with multiple parameters
sub fetch($$) {
    my ($param1, $param2) = @_;
    return "Fetch with multiple params: $param1, $param2";
}

1; # End of file 