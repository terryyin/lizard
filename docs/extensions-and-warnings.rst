Extensions and warning controls
===============================

`Back to the README <../README.rst>`_

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


Cognitive Complexity
--------------------

Cognitive Complexity (SonarSource, G. Ann Campbell) measures how hard a
function is to *understand* rather than how many paths it has: a
``switch`` counts one no matter how many cases it has, a sequence of like
logical operators (``a && b && c``) counts one, and control structures cost
more the deeper they are nested. Enable it as an extension; it adds a
``CogC`` column, a ``--CogC`` warning threshold (15 by default) and a
``cognitive_complexity`` field usable with ``-s`` and ``-T``:

::

   lizard -Ecognitive <path to your code>
   lizard -Ecognitive --CogC 25 -s cognitive_complexity <path to your code>

Nesting is followed for brace-delimited languages (C/C++, Java, C#,
JavaScript/TypeScript, Go, Rust, Kotlin, Swift, PHP, ...) and for Python;
for the other languages the increments are counted without the nesting
penalty. C preprocessor conditionals are not counted.


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
behavior of lizard. There are two types of forgiveness comments:

1. Function forgiveness: Put "#lizard forgives" inside a function or before a function to suppress warnings for that function.

::

   int foo() {
       // #lizard forgives
       ...
   }

   Selective forgiveness: Use "#lizard forgives(metric1, metric2)" to forgive only specific metrics (e.g. length, cyclomatic_complexity, parameter_count, nloc, token_count).

::

   int foo() {
       // #lizard forgives(length)  // Forgive only length violations
       ...
   }

2. Global code forgiveness: Put "#lizard forgive global" before global code to suppress warnings for all code outside of functions.

::

   // #lizard forgive global
   int global_var = 0;
   if (condition) {  // This complexity won't be counted
       ...
   }

   int foo() {  // Functions are still counted normally
       ...
   }
