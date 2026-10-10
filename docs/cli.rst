Command-line reference
======================

`Back to the README <../README.rst>`_

Options
-------

::

  -h, --help            show this help message and exit
  --version             show program's version number and exit
  -l LANGUAGES, --languages LANGUAGES
                        List the programming languages you want to analyze. if left empty, it'll
                        search for all languages it knows. `lizard -l cpp -l java`searches for
                        C++ and Java code. The available languages are: cpp, java, csharp,
                        javascript, python, objectivec, ttcn, ruby, php, swift, scala, GDScript,
                        go, lua, rust, typescript, plsql
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
  --no-gitignore        Do not use .gitignore files to exclude files.
  -t WORKING_THREADS, --working_threads WORKING_THREADS
                        number of working threads. The default value is 1. Using a bigger number
                        can fully utilize the CPU and often faster.
  -X, --xml             Generate XML in cppncss style instead of the tabular output. Useful to
                        generate report in Jenkins server
  --csv                 Generate CSV output as a transform of the default output
  -H, --html            Output HTML report with interactive DataTables (sortable, searchable, filterable)
  --checkstyle          Generate Checkstyle XML output for integration with Jenkins and other tools
  -m, --modified        Calculate modified cyclomatic complexity number , which count a
                        switch/case with multiple cases as one CCN.
  -E EXTENSIONS, --extension EXTENSIONS
                        User the extensions. The available extensions are: -Ecpre: it will ignore
                        code in the #else branch. -Ewordcount: count word frequencies and
                        generate tag cloud. -Eoutside: include the global code as one function.
                        -EIgnoreAssert: to ignore all code in assert. -ENS: count nested control
                        structures. -Ehalstead: compute Halstead complexity metrics. -Ecognitive:
                        compute Cognitive Complexity (SonarSource).
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
