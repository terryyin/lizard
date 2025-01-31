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