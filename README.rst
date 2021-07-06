|Web Site| Lizard
=================

.. image:: https://travis-ci.org/terryyin/lizard.png?branch=master
    :target: https://travis-ci.org/terryyin/lizard
.. image:: https://badge.fury.io/py/lizard.svg
    :target: https://badge.fury.io/py/lizard
.. |Web Site| image:: http://www.lizard.ws/website/static/img/logo-small.png
    :target: http://www.lizard.ws

|

Lizard is an extensible Cyclomatic Complexity Analyzer for many programming languages
including C/C++ (doesn't require all the header files or Java imports). It also does
copy-paste detection (code clone detection/code duplicate detection) and many other forms of static
code analysis.

A list of supported languages:

-  C/C++ (works with C++14)
-  Java
-  C# (C Sharp)
-  JavaScript (With ES6 and JSX)
-  TypeScript
-  Objective-C
-  Swift
-  Python
-  Ruby
-  TTCN-3
-  PHP
-  Scala
-  GDScript
-  Golang
-  Lua
-  Rust
-  Fortran

By default lizard will search for any source code that it knows and mix
all the results together. This might not be what you want. You can use
the "-l" option to select language(s).

It counts

-  the nloc (lines of code without comments),
-  CCN (cyclomatic complexity number),
-  token count of functions.
-  parameter count of functions.

You can set limitation for CCN (-C), the number of parameters (-a).
Functions that exceed these limitations will generate warnings. The exit
code of lizard will be none-Zero if there are warnings.

This tool actually calculates how complex the code 'looks' rather than
how complex the code really 'is'. People will need this tool because it's
often very hard to get all the included folders and files right when
they are complicated. But we don't really need that kind of accuracy for
cyclomatic complexity.

It requires python2.7 or above (early versions are not verified).

Installation
------------

lizard.py can be used as a stand alone Python script, most
functionalities are there. You can always use it without any
installation. To acquire all the functionalities of lizard, you will
need a proper install.

::

   python lizard.py

If you want a proper install:

::

   [sudo] pip install lizard

Or if you've got the source:

::

   [sudo] python setup.py install --install-dir=/path/to/installation/directory/

Usage
-----

::

   lizard [options] [PATH or FILE] [PATH] ...

Run for the code under current folder (recursively):

::

   lizard

Exclude anything in the tests folder:

::

    lizard mySource/ -x"./tests/*"


Options
~~~~~~~

::

  -h, --help            show this help message and exit
  --version             show program's version number and exit
  -l LANGUAGES, --languages LANGUAGES
                        List the programming languages you want to analyze. if left empty, it'll
                        search for all languages it knows. `lizard -l cpp -l java`searches for
                        C++ and Java code. The available languages are: cpp, java, csharp,
                        javascript, python, objectivec, ttcn, ruby, php, swift, scala, GDScript,
                        go, lua, rust, typescript
  -V, --verbose         Output in verbose mode (long function name)
  -C CCN, --CCN CCN     Threshold for cyclomatic complexity number warning. The default value is
                        15. Functions with CCN bigger than it will generate warning
  -f INPUT_FILE, --input_file INPUT_FILE
                        get a list of filenames from the given file
  -o OUTPUT_FILE, --output_file OUTPUT_FILE
                        Output file. The output format is inferred from the file extension (e.g.
                        .html), unless it is explicitly specified (e.g. using --xml).
  -L LENGTH, --length LENGTH
                        Threshold for maximum function length warning. The default value is 1000.
                        Functions length bigger than it will generate warning
  -a ARGUMENTS, --arguments ARGUMENTS
                        Limit for number of parameters
  -w, --warnings_only   Show warnings only, using clang/gcc's warning format for printing
                        warnings. http://clang.llvm.org/docs/UsersManual.html#cmdoption-
                        fdiagnostics-format
  --warning-msvs        Show warnings only, using Visual Studio's warning format for printing
                        warnings. https://msdn.microsoft.com/en-us/library/yxkt8b26.aspx
  -i NUMBER, --ignore_warnings NUMBER
                        If the number of warnings is equal or less than the number, the tool will
                        exit normally; otherwise, it will generate error. If the number is
                        negative, the tool exits normally regardless of the number of warnings.
                        Useful in makefile for legacy code.
  -x EXCLUDE, --exclude EXCLUDE
                        Exclude files that match the pattern. * matches everything, ? matches any
                        single character, "./folder/*" exclude everything in the folder
                        recursively. Multiple patterns can be specified. Don't forget to add ""
                        around the pattern.
  -t WORKING_THREADS, --working_threads WORKING_THREADS
                        number of working threads. The default value is 1. Using a bigger number
                        can fully utilize the CPU and often faster.
  -X, --xml             Generate XML in cppncss style instead of the tabular output. Useful to
                        generate report in Jenkins server
  --csv                 Generate CSV output as a transform of the default output
  -H, --html            Output HTML report
  -m, --modified        Calculate modified cyclomatic complexity number , which count a
                        switch/case with multiple cases as one CCN.
  -E EXTENSIONS, --extension EXTENSIONS
                        User the extensions. The available extensions are: -Ecpre: it will ignore
                        code in the #else branch. -Ewordcount: count word frequencies and
                        generate tag cloud. -Eoutside: include the global code as one function.
                        -EIgnoreAssert: to ignore all code in assert. -ENS: count nested control
                        structures.
  -s SORTING, --sort SORTING
                        Sort the warning with field. The field can be nloc,
                        cyclomatic_complexity, token_count, parameter_count, etc. Or an customized field.
  -T THRESHOLDS, --Threshold THRESHOLDS
                        Set the limit for a field. The field can be nloc, cyclomatic_complexity,
                        token_count, parameter_count, etc. Or an customized file. Lizard will
                        report warning if a function exceed the limit
  -W WHITELIST, --whitelist WHITELIST
                        The path and file name to the whitelist file. It's './whitelizard.txt' by
                        default. Find more information in README.


Example use
-----------

Analyze a folder recursively: lizard mahjong\_game/src
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

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

Warnings only (in clang/gcc formation):lizard -w mahjong\_game
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   ./src/html_ui/httpd.c:64: warning: accept_request has 19 CCN and 1 params (66 NLOC, 247 tokens)
   ./src/mahjong_game/mj_table.c:109: warning: mj_table_update_state has 20 CCN and 1 params (72 NLOC, 255 tokens)


Set warning threshold for any field:lizard -T nloc=25
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The option `-Tcyclomatic_complexity=10` is equal to `-C10`.
The option `-Tlength=10` is equal to `-L10`.
The option `-Tparameter_count=10` is equal to `-a10`.

You can also do `-Tnloc=10` to set the limit of the NLOC. Any function that
has NLOC greater than 10 will generate a warning.

Generated code
-----------------------------

Lizard has a simple solution with generated code. Any code in a source file that is following
a comment containing "GENERATED CODE" will be ignored completely. The ignored code will not
generate any data, except the file counting.


Code Duplicate Detector
-----------------------------

::

   lizard -Eduplicate <path to your code>


Generate A Tag Cloud For Your Code
----------------------------------

You can generate a "Tag cloud" of your code by the following command. It counts the identifiers in your code (ignoring the comments).

::

   lizard -EWordCount <path to your code>


Using lizard as Python module
-----------------------------

You can also use lizard as a Python module in your code:

.. code:: python

    >>> import lizard
    >>> i = lizard.analyze_file("../cpputest/tests/AllTests.cpp")
    >>> print i.__dict__
    {'nloc': 9, 'function_list': [<lizard.FunctionInfo object at 0x10bf7af10>], 'filename': '../cpputest/tests/AllTests.cpp'}
    >>> print i.function_list[0].__dict__
    {'cyclomatic_complexity': 1, 'token_count': 22, 'name': 'main', 'parameter_count': 2, 'nloc': 3, 'long_name': 'main( int ac , const char ** av )', 'start_line': 30}

You can also use source code string instead of file. But you need to
provide a file name (to identify the language).

.. code:: python

    >>> i = lizard.analyze_file.analyze_source_code("AllTests.cpp", "int foo(){}")

Whitelist
---------

If for some reason you would like to ignore the warnings, you can use
the whitelist. Add 'whitelizard.txt' to the current folder (or use -W to point to the whitelist file), then the
functions defined in the file will be ignored. Please notice that if you assign the file pathname, it needs to
be exactly the same relative path as Lizard to find the file. An easy way to get the file pathname is to copy it from
the Lizard warning output.
This is an example whitelist:

::

   #whitelizard.txt
   #The file name can only be whitelizard.txt and put it in the current folder.
   #You may have commented lines begin with #.
   function_name1, function_name2 # list function names in multiple lines or split with comma.
   file/path/name:function1, function2  # you can also specify the filename

Options in Comments
-------------------

You can use options in the comments of the source code to change the
behavior of lizard. By putting "#lizard forgives" inside a function or
before a function it will suppress the warning for that function.

::

   int foo() {
       // #lizard forgives the complexity
       ...
   }


Limitations
-----------

Lizard requires syntactically correct code.
Upon processing input with incorrect or unknown syntax:

- Lizard guarantees to terminate eventually (i.e., no forever loops, hangs)
  without hard failures (e.g., exit, crash, exceptions).

- There is a chance of a combination of the following soft failures:

    - omission
    - misinterpretation
    - improper analysis / tally
    - success (the code under consideration is not relevant, e.g., global macros in C)

This approach makes the Lizard implementation
simpler and more focused with partial parsers for various languages.
Developers of Lizard attempt to minimize the possibility of soft failures.
Hard failures are bugs in Lizard code,
while soft failures are trade-offs or potential bugs.

In addition to asserting the correct code,
Lizard may choose not to deal with some advanced or complicated language features:

- C/C++ digraphs and trigraphs are not recognized.
- C/C++ preprocessing or macro expansion is not performed.
  For example, using macro instead of parentheses (or partial statements in macros)
  can confuse Lizard's bracket stacks.
- Some C++ complicated templates may cause confusion with matching angle brackets
  and processing less-than ``<`` or more-than ``>`` operators
  inside of template arguments.


Literatures Referring to Lizard
-------------------------------

Lizard is often used in software related researches. If you used it to support your work, you may contact the lizard author to add your work in the following list.

- Software Quality in the ATLAS experiment at CERN, which refers to Lizard as one of the tools, has been published in the Journal of Physics: http://iopscience.iop.org/article/10.1088/1742-6596/898/7/072011

    - S Martin-Haugh et al 2017 J. Phys.: Conf. Ser. 898 072011

Lizard is also used as a plugin for fastlane to help check code complexity and submit xml report to sonar.

- `fastlane-plugin-lizard <https://github.com/liaogz82/fastlane-plugin-lizard>`_
- `sonar <https://github.com/Backelite/sonar-swift/blob/develop/docs/sonarqube-fastlane.md>`_
- `European research project FASTEN (Fine-grained Analysis of SofTware Ecosystems as Networks, <http://fasten-project.eu/)>`_
  - `for a quality analyzer <https://github.com/fasten-project/quality-analyzer>`_

