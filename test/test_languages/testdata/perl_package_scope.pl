#!/usr/bin/perl

use strict;
use warnings;

# First package
package FirstPackage;

# Package variable accessible by all functions in this package
our $package_var = 100;

# Function accessing package-level variable
sub access_package_var {
    return $package_var * 2;
}

# Function modifying package-level variable
sub modify_package_var {
    $package_var += 50;
    return $package_var;
}

# Function with its own lexical variable
sub with_lexical_var {
    my $local_var = 200;
    return $local_var + $package_var;
}

# Second package in the same file
package SecondPackage;

# This package has its own package variable
our $package_var = 500;

# Function accessing this package's variable
sub access_package_var {
    return $package_var * 3;
}

# Function accessing another package's variable
sub access_other_package {
    return $FirstPackage::package_var;
}

# Function with complex logic modifying package variables
sub complex_package_access {
    my ($param) = @_;
    
    if ($param > 0) {
        $package_var += $param;
        $FirstPackage::package_var += $param * 2;
    } else {
        $package_var -= 10;
    }
    
    return $package_var + $FirstPackage::package_var;
}

# Return to first package
package FirstPackage;

# Another function in the first package
sub another_function {
    return $package_var + 300;
}

1; # End of file 