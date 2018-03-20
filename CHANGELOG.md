# Change Log

## 1.14.7

in

  Class<?>[] testClasses = {String.class};

The ? wouldn't be counted as CC.

A minor fix for the --version option


## 1.14.0

Support CSV & HTML format output


## 1.13.0

No new feature. Just swtich to new pypi server.


### Added
- Option to ignore any warnings with '-i -1' (negative argument for the count).

## [0.12.7] - 2016-11-1
- 2016.04.2 Support PHP.
- 2016.03.26 Support C#.
- 2016.02.2 Add option -EMcCabe for ignoring fall-through swith/cases, thanks to @@vicgonzalez
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
