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

-  C# (C Sharp)
-  C/C++ (works with C++14)
-  Erlang
-  Fortran
-  GDScript
-  Golang
-  Java
-  JavaScript (With ES6 and JSX)
-  Kotlin
-  Lua
-  Objective-C
-  Perl
-  PHP
-  PL/SQL
-  Python
-  R
-  Ruby
-  Rust
-  Scala
-  Solidity
-  Structured Text (St)
-  Swift
-  TTCN-3
-  TypeScript (With TSX)
-  VueJS
-  Zig

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

It requires python3.8 or above (early versions are not verified).

JavaScript and TypeScript defaults
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Each default parameter or destructuring initializer adds 1 to CCN, because
the initializer runs only when the supplied value is ``undefined``. For example,
``function f(a = 1, b = a + 1) { return a + b; }`` has CCN 3: the base
complexity of 1 plus two defaults. Defaults in object and array destructuring
also count, including nested defaults and destructuring inside a function body.
Explicit decisions within an initializer, such as a ternary expression or
``&&`` / ``||``, contribute separately.

This also applies to JSX and TSX. Existing CCN scores can increase for functions
that use defaults, which may cause them to exceed a configured CCN threshold.

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

   [sudo] python setup.py install --prefix=/path/to/installation/directory/

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

Use .gitignore file:

::

    lizard mySource/

If there is a .gitignore file in the given path, lizard will automatically use it as an additional filter to exclude files that match the gitignore patterns. This is useful when you want to analyze only the tracked files in your git repository. To analyze all discovered source files regardless of .gitignore, use --no-gitignore:

::

    lizard --no-gitignore mySource/

.. _options:
.. _example-use:

Command-line reference
~~~~~~~~~~~~~~~~~~~~~~

See the `command-line reference
<https://github.com/terryyin/lizard/blob/master/docs/cli.rst>`_ for all
options, sample reports, and threshold examples.

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

.. _generated-code:
.. _code-duplicate-detector:
.. _generate-a-tag-cloud-for-your-code:
.. _cognitive-complexity:
.. _whitelist:
.. _options-in-comments:

Extensions and warning controls
-------------------------------

See `extensions and warning controls
<https://github.com/terryyin/lizard/blob/master/docs/extensions-and-warnings.rst>`_
for duplicate detection, word counts, Cognitive Complexity, generated
code, whitelists, and forgiveness comments.

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

How To Contribute
-----------------

Contributions are welcome. Project-specific development rules are in
``AGENTS.md``. Adding a language reader uses
``.agents/skills/lizard-language-support/``.

AI lifecycle guidance is installed under ``.agents/skills/`` and
``.claude/skills/``.
