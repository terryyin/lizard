######
Theory
######

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
