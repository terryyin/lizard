This project (Lizard) support JSX files, but the implementation is together with javascript support @javascript.py . Please extract the jsx support from the javascript support. Also update @testJSX.py and make sure @testJavaScript.py still pass.

---------

please add support for TSX file similar to @jsx.py and add test similar to @testJSX.py . Consider use @typescript.py and extract common part that support React syntax from @jsx.py . make sure the new test all pass. also refer to @javascript.py and @__init__.py 

---------

one test failed. It seems the problem is not in @tsx.py but the basic typescript @typescript.py support is incorrect. Please fix it. Make sure both @testTSX.py and @testTypeScript.py still pass.

---------

based on @tsx.py and @jsx.py , as well as @testTSX.py @testJSX.py , please add support for VueJS files, which should support both lang="js" and lang="ts". Don't forget @__init__.py 