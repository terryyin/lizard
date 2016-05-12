######
Theory
######

Cyclomatic Complexity
=========================

Counting the Cyclomatic Complexity Number is one of the most early and fundamental function
of Lizard. You can get more information about cycolmatic complexity from
`<https://en.wikipedia.org/wiki/Cyclomatic_complexity>`_.

The default way Lizard counts the CCN is mostly compatible with McCable's theory
(McCabe 1976 `<http://www.mccabe.com/pdf/mccabe-nist235r.pdf>`_), except that the
switch/case structure is counted by number of cases instead of switch. If you want
to strictly count by McCabe's definition, there's a Lizard extension to do that:

::

    lizard -Emaccabe

There's still an open issue that needs to be fixed. Forever loops, or loops without
a condition shouldn't be counted.

Below are some language specific notes about CCN.

CCN for Nested Structures
-------------------------

There can be nested structures in many languages. For Python, we could the CCN
for nested structure separately and it's not added to the parent structure.

For C++ (and C++-like languages) we ignored the nested structure with the outermost
function. All complexity (and perhaps all the other per-function statistics) are included
in the outermost function.

Nested Control Structures
=========================

Nested control-flow structures, such as ``if``, ``while``, ``for``, and ``switch``,
add more context to the current block of a code
and increase the indentation.
The complexity (# of control paths) of such block can be measured by its CCN,
which is done by the Nesting Depth metric.
In contrast, the Nested Control Structures metric
simply counts the nesting of deepest control structures within a function block.
For coding styles that enforce "one-line-one-statement" rule
and switch-case on the same indentation level (ex. Qt style),
this metric is equal to ``indentation_level - 1``
(without consideration of multi-line statements or line-breaks).

.. code-block:: cpp

    int foo()
    {
            while (expr1) {

                    for (; expr2;) {

                            for (; expr3;) {

                                    if (expr4)
                                        return 42;

                            }

                    }
            }
            return -42;
    }

The CCN of the above example would be less than 10,
and it may pass other quality checks;
however, it is likely to be considered too nested (4 structures)
by most coding conventions
and get refactored through extraction.

The default value for the maximum depth of nested structures is set to be 3.
The idea is taken from the Linux kernel coding style:

::

    The answer to that is that if you need more than 3 levels of indentation,
    you're screwed anyway, and should fix your program.
