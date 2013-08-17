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
Run for the code under current folder (recursively):
<pre>
hfcca
</pre>
Exclude anything in the tests folder:
<pre>
hfcca mySource/ -x"./tests/*"
</pre>
hfcca is a simple code complexity counter without caring about
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
                        Exclude files that match this pattern. * matches
                        everything, ? matches any single characoter,
                        "./folder/*" exclude everything in the folder,
                        recursively. Multiple patterns can be specified. Don't
                        forget to add "" around the pattern.
  -X, --xml             Generate XML in cppncss style instead of the normal
                        tabular output. Useful to generate report in Hudson
                        server
  -a ARGUMENTS, --arguments=ARGUMENTS
                        Limit for number of parameters
  -P, --no_preprocessor_count
                        By default, a #if will also increase the complexity.
                        Adding this option to ignore them
  -t WORKING_THREADS, --working_threads=WORKING_THREADS
                        number of working threads. The default value is 1.
</pre>                   
##Example use
### Analyze a folder recursively: hfcca mahjong_game/src
<pre>
==============================================================
  NLOC    CCN  token  param    function@line@file
--------------------------------------------------------------
    10      2     29      2    start_new_player@26@./html_game.c
   ...
     6      1      3      0    set_shutdown_flag@449@./httpd.c
    24      3     61      1    server_main@454@./httpd.c
--------------------------------------------------------------
2 file analyzed.
==============================================================
LOC    Avg.NLOC AvgCCN Avg.ttoken  function_cnt    file
--------------------------------------------------------------
    191     15      3        51        12     ./html_game.c
    363     24      4        86        15     ./httpd.c

======================================
!!!! Warnings (CCN > 15) !!!!
======================================
    66     19    247      1    accept_request@64@./httpd.c
=================================================================================
Total NLOC  Avg.NLOC  Avg CCN  Avg token  Fun Cnt  Warning cnt   Fun Rt   NLOC Rt  
--------------------------------------------------------------------------------
       554        20     4.07      71.15       27            1      0.04    0.12
</pre>
### Warnings only (in clang/gcc formation):hfcca -w mahjong_game
<pre>
./src/html_ui/httpd.c:64: warning: accept_request has 19 CCN and 1 params (66 NLOC, 247 tokens)
./src/mahjong_game/mj_table.c:109: warning: mj_table_update_state has 20 CCN and 1 params (72 NLOC, 255 tokens)
</pre>