This project (Lizard) support JSX files, but the implementation is together with javascript support @javascript.py . Please extract the jsx support from the javascript support. Also update @testJSX.py and make sure @testJavaScript.py still pass.

---------

please add support for TSX file similar to @jsx.py and add test similar to @testJSX.py . Consider use @typescript.py and extract common part that support React syntax from @jsx.py . make sure the new test all pass. also refer to @javascript.py and @__init__.py 

---------

one test failed. It seems the problem is not in @tsx.py but the basic typescript @typescript.py support is incorrect. Please fix it. Make sure both @testTSX.py and @testTypeScript.py still pass.

---------

based on @tsx.py and @jsx.py , as well as @testTSX.py @testJSX.py , please add support for VueJS files, which should support both lang="js" and lang="ts". Don't forget @__init__.py 

---------

add a new test for code
"""
        export default {
            methods: { 
                test(): number {
                    return 1;
                }
            }
        }
        """

run it and make the fix if the test fail. Please don't hard code the name 'methods'. It's just a nested object and can be anything.

---------

(all vue js related code removed. plan to try again.)

based on @tsx.py and @jsx.py , as well as @testTSX.py @testJSX.py , please add support for VueJS files, which should support both lang="js" and lang="ts". Don't forget @__init__.py 

1. to process vue code, a preprocess is probably needed like in @swift.py or @python.py ; don't add preprocess if not needed
2. it should identify if it's currently process the template, the style, or the script block
3. Then send the token to the cresponding sub reader to process
4. perhaps use parallel states as in @clike.py or @objc.py 
5. The vue reader shouldn't process js or ts content by itself but send to @javascript.py or @typescript.py 

run all new test and fix if the test fail. Add temporary debug info to understand what's going on first before applying fix when necessary.

---------

It looks like the XMLTagWithAttrTokenizer from @javascript.py belongs to @jsx.py , isn't it? Please move it out.

---------

@tsx.py @jsx.py should be doing very similar things. please refactor to remove the duplicate, inccluding conceptually duplicate code.

---------

@tsx.py and@jsx.py should be doing very similar things. But TSX has a TSXStates as parallel_states, but JSX doesn't need one. Why? please commonize the solution.

---------

TypeScript is a super set of JavaScript, right? If so, all test in @testJavaScript.py should also pass with a TypeScriptReader from @typescript.py . Please change @testJavaScript.py to use TypeScriptReader instead, run all the test. Then make the test pass by copying solution from @javascript.py to @typescript.py if it's missing there. The goal is to eventually remove @javascript.py . When runing tests, always run all the tests.

---------

TypeScript is a super set of JavaScript, right? If so, 

1. move Tokenizer and JSTokenizer from @javascript.py to @typescript.py; remove them from @javascript.py  ; update all the references. 
2. make JavaScriptReader in @javascript.py a subclass of TypeScriptReader
3. make sure all the test pass

---------

Tests in @testJavaScript.pyand @testES6.py  works fine if the `self.parallel_states = [JavaScriptStyleLanguageStates(context)]` in @javascript.py . However, if it is removed and just use the parallel_states from TypescriptReader from @typescript.py , some of the test will fail.

The TypeScriptStates from @typescript.py extends JavaScriptStyleLanguageStates from @js_style_language_states.py . But it seems instead of extending it, it also changed it's behavoir, which cause the tests to fail. This is considered a bug, as @typescript.py should be able to process JavaScript code as well.

please make @typescript.py extend JavaScriptStyleLanguageStates from @js_style_language_states.py instead of modify it. And the tests should pass. 

for the failure like `AssertionError: 'x' != '(anonymous)'` let's make the function name x intead of (anaymous) when there is an assignment.

run `python -m pytest test/test_languages/testES6.py` to check if the test pass.

---------

got the following issue. Please write a test to reproduce the problem. Run 'python -m pytest' to see if it actually fail.


Lizard version: 1.17.13

When I run the lizard tool with the --html flag and with the --output-file ending in .html, I get the error:

Warning: overriding output file extension.

Example:

lizard --length 75 \
		   --CCN 20 \
		   --arguments 3 \
		   --warnings_only \
		   --sort cyclomatic_complexity \
		   --html \
		   --output_file reports/complexity/complexity.html \
		   source
If I run it without the .html extension, the warning goes away but the output file does not have the .html extension despite being formatted like an html file.

---------


To support java 23. Please add these test cases to @testJava.py :

test_sealed_class_with_permits_clause
test_sealed_interface_with_permits_clause
test_lambda_expression_simple
test_lambda_expression_with_multiple_parameters
test_method_reference_expression
test_record_compact_constructor
test_enum_declaration_with_methods
test_local_class_inside_method
test_switch_expression_with_yield
test_pattern_matching_instanceof

---------

Got the following issue. Please:

1. find where is the right place to add a new unit test to reproduce the issue
2. add a new unit test
3. run all test to see if the test really fails
4. fix the issue until all test pass
5. clean up
6. run all test at least to confirm things are still working.

-------------
The issue:

I am using lizard in a tool I manage called Statick. When we run lizard we use the -w flag by default, and we are seeing lizard crash. I am seeing the same behavior when running the lizard tool on its own. This behavior does not happen in v1.17.10, but it does occur in 1.17.17. I have narrowed it down to a change between v1.17.14 and v1.17.15, persisting on through v1.17.17.

A minimal example is to use the C file at the bottom of this post as input. Save it as test.c. Then run

lizard test.c
That runs fine. Now add the -w flag. This crashes with Traceback

$ lizard --version
1.17.17
$ lizard -w test.c 
Traceback (most recent call last):
  File "/home/thomas/.local/bin/lizard", line 8, in <module>
    sys.exit(main())
  File "/home/thomas/.local/lib/python3.10/site-packages/lizard.py", line 1079, in main
    warning_count = printer(result, options, schema, AllResult)
  File "/home/thomas/.local/lib/python3.10/site-packages/lizard.py", line 855, in print_clang_style_warning
    print(scheme.clang_warning_format().format(f=warning))
AttributeError: 'FunctionInfo' object has no attribute 'max_nesting_depth'
In v1.17.14 there is no problem and I see the warning.

$ lizard --version
1.17.14
$ lizard -w test.c 
test.c:2: warning: func has 52 NLOC, 16 CCN, 143 token, 0 PARAM, 69 length
The test.c file I am using is

#include <stdlib.h>
void func()
{
  int a, b, c, d, e, f, g, h, i, j, k, l, m, n, o;

  if (a)
  {
    // Do nothing.
  }
  else if (b)
  {
    // Do nothing.
  }
  else if (c)
  {
    // Do nothing.
  }
  else if (d)
  {
    // Do nothing.
  }
  else if (e)
  {
    // Do nothing.
  }
  else if (f)
  {
    // Do nothing.
  }
  else if (g)
  {
    // Do nothing.
  }
  else if (h)
  {
    // Do nothing.
  }
  else if (i)
  {
    // Do nothing.
  }
  else if (j)
  {
    // Do nothing.
  }
  else if (k)
  {
    // Do nothing.
  }
  else if (l)
  {
    // Do nothing.
  }
  else if (m)
  {
    // Do nothing.
  }
  else if (n)
  {
    // Do nothing.
  }
  else if (o)
  {
    // Do nothing.
  }
  else
  {
    // Do nothing.
  }
}