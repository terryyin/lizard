# Change Log

## 1.17.5

Bug fixing in function parameter names. Previously if the parameter has
default value, it will take the value as the parameter name.

## 1.17.3

Bug fixing in Lua and Scala support

## 1.17.2

Fix missing file when install

## 1.17.1

Fix HTML Output template problem.

## 1.17.0

Support JavaScript with ES6 and JSX.
Support Rust

## 1.16.6

Bug fixes:

'for' keyword in Swift label should not be counted as ccn.

'guard' keyword should be counted.

'-o' or '--output_file' parameter added.


## 1.16.3

Fix the performance issue

## 1.16.1

Add Lua to the languages.

## 1.15.7

Fix a bug in MSVS format output.

## 1.15.6

Minor fix for the average NCSS (lines of code) in the XML output.
Previously, the average NCSS per function included code outside any
function. Now this is fixed.


## 1.15.2

An important new feature -- duplicate detector -- in the form of an
extension.

Usage:

    lizard -Eduplicate

The current version hasn't had any performance optimization yet. Yet, it
can scan 1000 cpp files in less than 2 minutes.

Code after comments containing "GENERATED CODE" will be ignored.

## 1.14.10

in

  Class<?>[] testClasses = {String.class};

The ? wouldn't be counted as CC.

A minor fix for the --version option


## 1.14.0

Support CSV & HTML format output


## 1.13.0

No new feature. Just switch to new pypi server.


### Added
- Option to ignore any warnings with '-i -1' (negative argument for the count).

## [0.12.7] - 2016-11-1
- 2016.04.2 Support PHP.
- 2016.03.26 Support C#.
- 2016.02.2 Add option -EMcCabe for ignoring fall-through switch/cases, thanks to @@vicgonzalez
- 2016.01.31 Add support for Ruby
- 2016.01.29 Add -T option to set limit for any field
- 2015.12.17 Add support for Swift
- 2015.12.12 Add the -l option to filter language
- 2015.10.22 TTCN-3 added by @gustafj
- 2015.10.06 Add C++11 uniform constructor initialization. Thanks to @rakhimov
- 2015.01.09 Add C preprocessor back by -Ecpre. it will ignore all the #else branch in the C/C++ code.
- 2015.01.07 pass test for linux kernal and other popular open source C/C++ code.
- 2014.04.07 Remove option -e (display function end line), and make it default
- 2014.04.06 Remove option -d (ignore duplicated content), and make it default
- 2014.04.06 Remove option -p (no preprocessor count), and a '#if' will always be counted in cyclomatic complexity
- 2014.03.31 Support JavaScript!
- 2014.03.22 Change the -v (--verbose) option to -V. This is because -v
  will be used for --version.
