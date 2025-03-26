#!/usr/bin/perl

use strict;
use warnings;

package OneLinerTest;

# Basic one-liner function with no body
sub empty_oneliner;

# One-liner function with definition
sub simple_oneliner { return 42; }

# One-liner with parameter
sub param_oneliner { my ($x) = @_; return $x * 2; }

# One-liner with condition
sub condition_oneliner { my ($x) = @_; return $x > 0 ? "positive" : "negative"; }

# One-liner with multiple statements 
sub multi_statement_oneliner { my ($x) = @_; $x++; return $x; }

# Package method one-liner
package AnotherPackage;
sub pkg_oneliner { return "package"; }

1; # End of package 