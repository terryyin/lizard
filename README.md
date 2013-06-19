#Header Free Cyclomatic Complexity Analyzer
[![Build Status](https://travis-ci.org/terryyin/hfcca.png?branch=master)](https://travis-ci.org/terryyin/hfcca)

This tool will calculate the cycolomatic complexity of C/C++/Objective C code without caring about header files and preprocessors. So the tool is actually calculating how complex the code 'looks' rather than how complex the code 'is'.

People will need this tool because it's often very hard to get all the include folders and files right with a similar tool, but we don't really need that kind of accuracy when come to cyclomatic complexity.

##Installation

hfcca.py is only a single python file, you can always use it without install.

If you want a proper install:

<pre>
[sudo] pip install hfcca
</pre>

Or if you've got the source:

<pre>
[sudo] python setup.py install
</pre>

##Usage

<pre>
hfcca [options] [PATH or FILE] [PATH] ... 
</pre>

hfcca (verion 1.5) is a simple code complexity counter without caring about
the C/C++ header files. It can deal with C/C++/ObjectiveC & TNSDL code. It count the NLOC
(lines of code without comments), CCN  (cyclomatic complexity number) and
token count of functions.

It requires python2.6 or above (early versions are not verified).

<pre>
Options:
  -h, --help            show this help message and exit
  -v, --verbose         Output in verbose mode (long function name)
  -C CCN, --CCN=CCN     Threshold for cyclomatic complexity number warning.
                        functions with CCN bigger than this number will be
                        shown in warning
  -w, --warnings_only   Show warnings only
  -i NUMBER, --ignore_warnings=NUMBER
                        If the number of warnings is equal or less than the
                        number, the tool will exit normally, otherwize it will
                        generate error. Useful in makefile when improving
                        legacy code.
  -x EXCLUDE, --exclude=EXCLUDE
                        Exclude data files that match this regular expression
  -X, --xml             Generate XML in cppncss style instead of the normal
                        tabular output. Useful to generate report in Hudson
                        server
  -p, --preprocess      Use preprocessor, always ignore the #else branch. By
                        default, source_analyzer just ignore any preprocessor
                        statement.
  -a ARGUMENTS, --arguments=ARGUMENTS
                        Limit for number of parameters
  -P, --no_preprocessor_count
                        By default, a #if will also increase the complexity.
                        Adding this option to ignore them
  -t WORKING_THREADS, --working_threads=WORKING_THREADS
                        number of working threads. The default value is 1.
</pre>                   
