# Consumer trace

## Effect terms searched

| Term | Origin |
| --- | --- |
| `TypeScriptReader` | supplied |
| `TSXReader` | supplied |
| `JavaScriptReader` | supplied |
| `VueReader` | supplied |
| `get_js_function_list` | supplied |
| `get_ts_function_list` | supplied |
| `parameter_count` | supplied |

## Scope

Whole checkout at `c3ca2c73d3c7`, including uncommitted and untracked files: 1181 tracked and untracked non-ignored files searched with fixed-string, case-insensitive matching.

Changed files not searched: none named.

## Located consumers

Kinds are path hints for inspection; each file appears once. Fill every Disposition with `run`, `reused` or `excluded: <reason the effect does not reach its boundary>`.

### Production

| File | Terms matched | First matching line | Disposition |
| --- | --- | --- | --- |
| `docs/cli.rst` | `parameter_count` | 67: `cyclomatic_complexity, token_count, parameter_count, etc. Or an customized field.` |excluded: no execution of or observation of this reader effect |
| `docs/extensions-and-warnings.rst` | `parameter_count` | 87: `Selective forgiveness: Use "#lizard forgives(metric1, metric2)" to forgive only specific metrics (e.g. length, cyclomatic_complexity, parameter_count, nloc, to…` |excluded: no execution of or observation of this reader effect |
| `docs/reviews/fabio-malpezzi-20261027/paired.py` | `parameter_count` | 34: `function.parameter_count` |reused: historical executor replay remains pinned to its recorded immutable source/harness; current harness unchanged |
| `docs/reviews/fabio-malpezzi-20261027/pr528/profile-control.py` | `parameter_count` | 20: `return [(f.name, f.cyclomatic_complexity, f.token_count, f.parameter_count,` |reused: historical executor replay remains pinned to its recorded immutable source/harness; current harness unchanged |
| `docs/reviews/fabio-malpezzi-20261027/pr528/proposed-patch.json` | `get_js_function_list` | 17: `"patch": "@@ -2,6 +2,8 @@\n \n from lizard import analyze_file\n \n+from .testJavaScript import get_js_function_list\n+\n \n class TestJavaScriptRegex(unittest…` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr531/531-cli-reproduction-readonly-function_type_string.ts.log` | `parameter_count` | 16: `No thresholds exceeded (cyclomatic_complexity > 15 or length > 1000 or nloc > 1000000 or parameter_count > 100)` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr531/531-cli-reproduction-readonly-reported.js.log` | `parameter_count` | 15: `No thresholds exceeded (cyclomatic_complexity > 15 or length > 1000 or nloc > 1000000 or parameter_count > 100)` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr531/531-cli-reproduction-target-function_type_string.ts.log` | `parameter_count` | 16: `No thresholds exceeded (cyclomatic_complexity > 15 or length > 1000 or nloc > 1000000 or parameter_count > 100)` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr531/531-cli-reproduction-target-reported.js.log` | `parameter_count` | 15: `No thresholds exceeded (cyclomatic_complexity > 15 or length > 1000 or nloc > 1000000 or parameter_count > 100)` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr531/profile-control.py` | `parameter_count` | 23: `observed = [(f.name, f.cyclomatic_complexity, f.token_count, f.parameter_count,` |reused: historical executor replay remains pinned to its recorded immutable source/harness; current harness unchanged |
| `docs/reviews/fabio-malpezzi-20261027/pr531/proposed-patch.json` | `get_js_function_list` | 13: `"patch": "@@ -511,3 +511,35 @@ def test_arrow_with_block_and_return(self):\n         functions = get_js_function_list(code)\n         self.assertEqual([\"valid…` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr531/reproduce.py` | `parameter_count` | 27: `f.parameter_count] for f in functions]}))` |reused: historical executor replay remains pinned to its recorded immutable source/harness; current harness unchanged |
| `docs/reviews/fabio-malpezzi-20261027/pr532/full-patch.json` | `get_js_function_list`, `get_ts_function_list`, `parameter_count` | 1: `{"base":"01afe9c228ba2a3dc5253259d8415d2ef47f0b71","head":"d0713bf5b4dcf4bf24453156a3756688627abbc0","patch":"diff --git a/lizard_languages/javascript_function…` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr532/github-patch.json` | `get_js_function_list`, `get_ts_function_list`, `parameter_count` | 1: `{"patches":[{"filename":"lizard_languages/javascript_functions.py","patch":"@@ -3,13 +3,6 @@\n from .javascript_bindings import JavaScriptBindings\n \n \n-# Ty…` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr532/github-pr.json` | `get_js_function_list`, `get_ts_function_list`, `parameter_count` | 1: `{"pull_request":{"url":"https://github.com/terryyin/lizard/pull/532","number":532,"state":"open","merged":false,"mergeable":true,"draft":false,"body":"### The …` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr532/observe-website.py` | `parameter_count` | 30: `assert (function['name'], function['parameter_count']) == (name, count), function` |reused: historical executor replay remains pinned to its recorded immutable source/harness; current harness unchanged |
| `docs/reviews/fabio-malpezzi-20261027/pr532/raw-target-focused.part1.log` | `parameter_count` | 130: `>       assert function.parameter_count == 2` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr532/raw-target-focused.part2.log` | `parameter_count` | 2: `>       assert function.parameter_count == 2` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr532/raw-target-red.part1.log` | `parameter_count` | 110: `>       assert function.parameter_count == 2` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr532/raw-target-red.part2.log` | `parameter_count` | 1: `E        +  where 1 = <lizard_core.function_info.FunctionInfo object at 0x1047ca0a0>.parameter_count` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr532/reproduce.py` | `parameter_count` | 24: `actual = [[f.name, f.parameter_count] for f in functions]` |reused: historical executor replay remains pinned to its recorded immutable source/harness; current harness unchanged |
| `docs/reviews/fabio-malpezzi-20261027/pr532/target-red.log` | `parameter_count` | 112: `>       assert [(f.name, f.parameter_count) for f in result] == [` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr533/authored-tests.py` | `get_js_function_list` | 5: `from test.test_languages.testES6 import get_js_function_list` |reused: historical executor replay remains pinned to its recorded immutable source/harness; current harness unchanged |
| `docs/reviews/fabio-malpezzi-20261027/pr533/independent-child-cli-async_field.js.log` | `parameter_count` | 14: `No thresholds exceeded (cyclomatic_complexity > 15 or length > 1000 or nloc > 1000000 or parameter_count > 100)` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr533/independent-child-cli-inherited_function_type_string.ts.log` | `parameter_count` | 15: `No thresholds exceeded (cyclomatic_complexity > 15 or length > 1000 or nloc > 1000000 or parameter_count > 100)` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr533/independent-child-cli-multiplication.js.log` | `parameter_count` | 13: `No thresholds exceeded (cyclomatic_complexity > 15 or length > 1000 or nloc > 1000000 or parameter_count > 100)` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr533/independent-child-cli-reported.js.log` | `parameter_count` | 15: `No thresholds exceeded (cyclomatic_complexity > 15 or length > 1000 or nloc > 1000000 or parameter_count > 100)` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr533/proposed-patch.json` | `get_js_function_list` | 1: `{"base":"01afe9c228ba2a3dc5253259d8415d2ef47f0b71","head":"15423852c04aed302f684395fe9522251d88a2f2","diff":"diff --git a/lizard_languages/javascript_functions…` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr533/raw-cli-async_field.js.log` | `parameter_count` | 14: `No thresholds exceeded (cyclomatic_complexity > 15 or length > 1000 or nloc > 1000000 or parameter_count > 100)` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr533/raw-cli-inherited_function_type_string.ts.log` | `parameter_count` | 15: `No thresholds exceeded (cyclomatic_complexity > 15 or length > 1000 or nloc > 1000000 or parameter_count > 100)` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr533/raw-cli-multiplication.js.log` | `parameter_count` | 13: `No thresholds exceeded (cyclomatic_complexity > 15 or length > 1000 or nloc > 1000000 or parameter_count > 100)` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr533/raw-cli-reported.js.log` | `parameter_count` | 15: `No thresholds exceeded (cyclomatic_complexity > 15 or length > 1000 or nloc > 1000000 or parameter_count > 100)` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr533/target-authored.log` | `get_js_function_list` | 150: `with_semicolon = get_js_function_list(` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr533/target-cli-async_field.js.log` | `parameter_count` | 12: `No thresholds exceeded (cyclomatic_complexity > 15 or length > 1000 or nloc > 1000000 or parameter_count > 100)` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr533/target-cli-inherited_function_type_string.ts.log` | `parameter_count` | 15: `No thresholds exceeded (cyclomatic_complexity > 15 or length > 1000 or nloc > 1000000 or parameter_count > 100)` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr533/target-cli-multiplication.js.log` | `parameter_count` | 13: `No thresholds exceeded (cyclomatic_complexity > 15 or length > 1000 or nloc > 1000000 or parameter_count > 100)` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr533/target-cli-reported.js.log` | `parameter_count` | 15: `No thresholds exceeded (cyclomatic_complexity > 15 or length > 1000 or nloc > 1000000 or parameter_count > 100)` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr534/child-cli-curried_internal_control.js.log` | `parameter_count` | 15: `No thresholds exceeded (cyclomatic_complexity > 15 or length > 1000 or nloc > 1000000 or parameter_count > 100)` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr534/child-cli-inherited_function_type_string.ts.log` | `parameter_count` | 15: `No thresholds exceeded (cyclomatic_complexity > 15 or length > 1000 or nloc > 1000000 or parameter_count > 100)` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr534/child-cli-reported.js.log` | `parameter_count` | 14: `No thresholds exceeded (cyclomatic_complexity > 15 or length > 1000 or nloc > 1000000 or parameter_count > 100)` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr534/child-cli-typed_arrows.ts.log` | `parameter_count` | 14: `No thresholds exceeded (cyclomatic_complexity > 15 or length > 1000 or nloc > 1000000 or parameter_count > 100)` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr534/proposed-patch.json` | `get_js_function_list` | 1: `{"base": "01afe9c228ba2a3dc5253259d8415d2ef47f0b71", "head": "f4074ae860c7999cc37dd081a6f7ebfa777adc83", "patch": "diff --git a/lizard_languages/javascript_fun…` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr534/raw-cli-curried_internal_control.js.log` | `parameter_count` | 15: `No thresholds exceeded (cyclomatic_complexity > 15 or length > 1000 or nloc > 1000000 or parameter_count > 100)` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr534/raw-cli-inherited_function_type_string.ts.log` | `parameter_count` | 15: `No thresholds exceeded (cyclomatic_complexity > 15 or length > 1000 or nloc > 1000000 or parameter_count > 100)` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr534/raw-cli-reported.js.log` | `parameter_count` | 14: `No thresholds exceeded (cyclomatic_complexity > 15 or length > 1000 or nloc > 1000000 or parameter_count > 100)` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr534/raw-cli-typed_arrows.ts.log` | `parameter_count` | 14: `No thresholds exceeded (cyclomatic_complexity > 15 or length > 1000 or nloc > 1000000 or parameter_count > 100)` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr534/target-cli-curried_internal_control.js.log` | `parameter_count` | 14: `No thresholds exceeded (cyclomatic_complexity > 15 or length > 1000 or nloc > 1000000 or parameter_count > 100)` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr534/target-cli-inherited_function_type_string.ts.log` | `parameter_count` | 15: `No thresholds exceeded (cyclomatic_complexity > 15 or length > 1000 or nloc > 1000000 or parameter_count > 100)` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr534/target-cli-reported.js.log` | `parameter_count` | 13: `No thresholds exceeded (cyclomatic_complexity > 15 or length > 1000 or nloc > 1000000 or parameter_count > 100)` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr534/target-cli-typed_arrows.ts.log` | `parameter_count` | 14: `No thresholds exceeded (cyclomatic_complexity > 15 or length > 1000 or nloc > 1000000 or parameter_count > 100)` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr535/child.diff` | `get_ts_function_list`, `parameter_count` | 31: `[(f.name, f.parameter_count) for f in functions])` |excluded: proof/trace record itself; no product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr535/focused.py` | `TypeScriptReader`, `TSXReader`, `JavaScriptReader`, `VueReader`, `get_js_function_list`, `get_ts_function_list`, `parameter_count` | 13: `['--term', 'TypeScriptReader', '--term', 'TSXReader',` |run: literal README commands; bootstrap uses immutable Git reader bytes, never normalized display patches |
| `docs/reviews/fabio-malpezzi-20261027/pr535/full.diff` | `get_js_function_list`, `get_ts_function_list`, `parameter_count` | 74: `functions = get_js_function_list("function foo(a, b){}")` |excluded: proof/trace record itself; no product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr535/github-patch.json` | `get_js_function_list`, `get_ts_function_list`, `parameter_count` | 1: `{"patches":[{"filename":"lizard_languages/javascript_functions.py","patch":"@@ -3,13 +3,6 @@\n from .javascript_bindings import JavaScriptBindings\n \n \n-# Ty…` |excluded: proof/trace record itself; no product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr535/github-pr.json` | `get_js_function_list`, `get_ts_function_list`, `parameter_count` | 1: `{"pull_request":{"url":"https://github.com/terryyin/lizard/pull/535","number":535,"state":"open","merged":false,"mergeable":true,"draft":false,"body":"Depends …` |excluded: proof/trace record itself; no product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr535/reproduce.py` | `parameter_count` | 27: `actual = [[f.name, f.parameter_count] for f in result.function_list]` |run: literal README commands; bootstrap uses immutable Git reader bytes, never normalized display patches |
| `index.html` | `parameter_count` | 94: `<td align=right>{{fun.parameter_count}}</td>` |excluded: no execution of or observation of this reader effect |
| `lizard_core/function_info.py` | `parameter_count` | 65: `parameter_count = property(lambda self: len(self.parameters))` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `lizard_core/options.py` | `parameter_count` | 188: `parameter_count, etc. Or an customized field.''',` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `lizard_core/output_scheme.py` | `parameter_count` | 39: `{'caption': " PARAM ", 'value': "parameter_count"},` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `lizard_ext/htmloutput.py` | `parameter_count` | 157: `<th>Parameter count ({{ thresholds["parameter_count"] }})</th>` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `lizard_languages/__init__.py` | `TypeScriptReader`, `TSXReader`, `JavaScriptReader`, `VueReader` | 7: `from .javascript import JavaScriptReader` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `lizard_languages/javascript.py` | `TypeScriptReader`, `JavaScriptReader` | 5: `from .typescript import TypeScriptReader` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `lizard_languages/tsx.py` | `TypeScriptReader`, `TSXReader` | 10: `JS_IDENTIFIER, NULLISH_COALESCING, OPTIONAL_ACCESS, TypeScriptReader)` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `lizard_languages/typescript.py` | `TypeScriptReader` | 58: `class TypeScriptReader(CodeReader, CCppCommentsMixin):` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `lizard_languages/vue.py` | `TypeScriptReader`, `VueReader` | 6: `from .typescript import TypeScriptReader` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `README.rst` | `parameter_count` | 161: `{'cyclomatic_complexity': 1, 'token_count': 22, 'name': 'main', 'parameter_count': 2, 'nloc': 3, 'long_name': 'main( int ac , const char ** av )', 'start_line'…` |excluded: guidance/history names the concept but does not execute the reader |
| `website/static/controllers.js` | `parameter_count` | 20: `$scope.result = {"token_count":307,"filename":".c","function_list":[{"parameter_count":1,"end_line":8,"nloc":3,"cyclomatic_complexity":1,"start_line":6,"token_…` |excluded: legacy selected-checkout controller was removed from current target; actual adapter traced separately |

### Test

| File | Terms matched | First matching line | Disposition |
| --- | --- | --- | --- |
| `test/test_analyzer.py` | `parameter_count` | 136: `fun.forgiven_metrics = {'length', 'parameter_count'}` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `test/test_extensions/testCognitiveComplexity.py` | `parameter_count` | 723: `"token_count", "parameter_count", "start_line",` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `test/test_languages/testCAndCPP.py` | `parameter_count` | 119: `self.assertEqual(0, result[0].parameter_count)` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testES6.py` | `JavaScriptReader`, `get_js_function_list` | 3: `from lizard_languages import JavaScriptReader` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `test/test_languages/testFortranProcedureBoundaries.py` | `parameter_count` | 36: `(first.parameter_count, second.parameter_count))` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testGo.py` | `parameter_count` | 31: `self.assertEqual(0, result[0].parameter_count)` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testGoDeclarations.py` | `parameter_count` | 14: `self.assertEqual(1, result[0].parameter_count)` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testGoFunctionTypes.py` | `parameter_count` | 49: `assert result[0].parameter_count == 1` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testJavaScript.py` | `TypeScriptReader`, `get_js_function_list`, `parameter_count` | 4: `from lizard_languages import TypeScriptReader` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `test/test_languages/testJavaScriptFalsePositives.py` | `get_js_function_list` | 3: `from .testJavaScript import get_js_function_list` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `test/test_languages/testJavaScriptFunctions.py` | `get_js_function_list`, `parameter_count` | 3: `from .testJavaScript import get_js_function_list` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `test/test_languages/testJavaScriptMembers.py` | `get_js_function_list` | 3: `from .testJavaScript import get_js_function_list` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `test/test_languages/testJavaScriptParameterNames.py` | `parameter_count` | 24: `assert function.parameter_count == 9` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `test/test_languages/testJavaScriptWidgets.py` | `get_js_function_list` | 3: `from .testJavaScript import get_js_function_list` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `test/test_languages/testKotlin.py` | `parameter_count` | 37: `self.assertEqual(0, result[0].parameter_count)` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testKotlinExpressionBody.py` | `parameter_count` | 21: `self.assertEqual([1, 1, 1], [f.parameter_count for f in result])` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testLua.py` | `parameter_count` | 52: `self.assertEqual(0, result[0].parameter_count)` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testOptionalOperators.py` | `parameter_count` | 104: `assert (function.name, function.parameter_count,` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `test/test_languages/testPHP.py` | `parameter_count` | 97: `def test_parameter_count(self):` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testPLSQL.py` | `parameter_count` | 56: `self.assertEqual(2, result[0].parameter_count)` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testPython.py` | `parameter_count` | 44: `def test_parameter_count(self):` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testPythonParameters.py` | `parameter_count` | 7: `def test_parameter_count_with_comma_in_default_value(self):` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testPythonTypeParameters.py` | `parameter_count` | 40: `self.assertEqual(plain.parameter_count, generic.parameter_count)` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testRuby.py` | `parameter_count` | 119: `self.assertEqual(0, result[0].parameter_count)` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testScala.py` | `parameter_count` | 32: `self.assertEqual(0, result[0].parameter_count)` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testSwift.py` | `parameter_count` | 35: `self.assertEqual(0, result[0].parameter_count)` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testTSX.py` | `TSXReader` | 3: `from lizard_languages import TSXReader` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `test/test_languages/testTTCN.py` | `parameter_count` | 221: `self.assertEqual(0, result[0].parameter_count)` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testTypeScript.py` | `TypeScriptReader`, `get_ts_function_list`, `parameter_count` | 4: `from lizard_languages import TypeScriptReader` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `test/test_languages/testTypeScriptArrowFields.py` | `get_ts_function_list` | 3: `from .testTypeScript import get_ts_function_list` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `test/test_languages/testTypeScriptDeclarations.py` | `get_ts_function_list` | 3: `from .testTypeScript import get_ts_function_list` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `test/test_languages/testTypeScriptFalsePositives.py` | `get_ts_function_list` | 3: `from .testTypeScript import get_ts_function_list` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `test/test_languages/testTypeScriptFields.py` | `get_ts_function_list` | 3: `from .testTypeScript import get_ts_function_list` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `test/test_languages/testTypeScriptFunctions.py` | `get_ts_function_list` | 3: `from .testTypeScript import get_ts_function_list` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `test/test_languages/testTypeScriptMembers.py` | `get_ts_function_list` | 3: `from .testTypeScript import get_ts_function_list` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `test/test_languages/testTypeScriptMetrics.py` | `get_ts_function_list`, `parameter_count` | 3: `from .testTypeScript import get_ts_function_list` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `test/test_languages/testTypeScriptThisParameters.py` | `parameter_count` | 28: `assert [(f.name, f.parameter_count) for f in result] == [` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `test/test_languages/testTypeScriptWidgets.py` | `get_ts_function_list` | 3: `from .testTypeScript import get_ts_function_list` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `test/test_languages/testVue.py` | `VueReader` | 3: `from lizard_languages import VueReader` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `test/test_languages/testZig.py` | `parameter_count` | 25: `self.assertEqual(0, result[0].parameter_count)` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_options.py` | `parameter_count` | 55: `self.assertEqual(123, options.thresholds['parameter_count'])` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `test/testCommentOptions.py` | `parameter_count` | 30: `"void foo(){/* #lizard forgives(length, parameter_count) */}")` |excluded: C++ comment forgiveness fixture cannot enter the JS parameter reader |
| `test/testLanguages.py` | `JavaScriptReader` | 2: `from lizard_languages import get_reader_for, CLikeReader, JavaReader, ObjCReader, JavaScriptReader, ScalaReader,\` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |

### Test support or stand-in

none found

### Journey or feature

none found

### Guidance reader

| File | Terms matched | First matching line | Disposition |
| --- | --- | --- | --- |
| `.planning/slice-plans/004-fabio-malpezzi-pr-reviews/CONTEXT.md` | `TypeScriptReader`, `parameter_count` | 8: `- JS inherits TypeScriptReader. TypeScriptStates composes function, binding,` |excluded: guidance/history names the concept but does not execute the reader |
| `.planning/slice-plans/004-lizard-website-revival/PLAN.md` | `parameter_count` | 66: `` `nix develop -c python -c 'from lizard import analyze_file; result=analyze_file.analyze_source_code("example.py", "def classify(value):\n    if value > 0:\n   … `` |excluded: guidance/history names the concept but does not execute the reader |
| `CHANGELOG.md` | `parameter_count` | 158: `` - Use `#lizard forgives(length, parameter_count)` for multiple metrics `` |excluded: guidance/history names the concept but does not execute the reader |
| `docs/reviews/fabio-malpezzi-20261027/pr531/review-findings.md` | `TypeScriptReader`, `TSXReader`, `JavaScriptReader`, `VueReader` | 107: `\| JavaScriptReader → TypeScriptReader/TypeScriptStates \| run: .js API/CLI reproduction and focused JavaScript/ES6 suites \|` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr532/consumer-supplement.md` | `get_js_function_list`, `get_ts_function_list` | 38: `` \| `get_js_function_list` \| supplied \| `` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr532/consumer-trace-1.md` | `parameter_count` | 47: `` \| `parameter_count` \| supplied \| `` |excluded: proof/trace record itself; no product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr532/consumer-trace-2.md` | `parameter_count` | 1: `` \| `docs/reviews/fabio-malpezzi-20261027/pr533/raw-cli-reported.js.log` \| `parameter_count` \| 15: `No thresholds exceeded (cyclomatic_complexity > 15 or length … `` |excluded: proof/trace record itself; no product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr534/review-findings.md` | `TypeScriptReader` | 97: `\| Vue script reader inheriting TypeScriptReader \| Run: six Vue controls including reported, typed and inherited literal cases \|` |excluded: historical evidence snapshot, not a current product caller |
| `prompt-history.md` | `TypeScriptReader`, `JavaScriptReader` | 58: `TypeScript is a super set of JavaScript, right? If so, all test in @testJavaScript.py should also pass with a TypeScriptReader from @typescript.py . Please cha…` |excluded: no execution of or observation of this reader effect |
