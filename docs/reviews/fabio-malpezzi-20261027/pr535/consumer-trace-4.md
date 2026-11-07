# Consumer trace

## Effect terms searched

| Term | Origin |
| --- | --- |
| `parameter_count` | supplied |
| `TypeScriptReader` | supplied |
| `.parameters` | supplied |
| `full_parameters` | supplied |
| `function-results` | supplied |

## Scope

Whole checkout at `9bc44563aad4`, including uncommitted and untracked files: 899 tracked and untracked non-ignored files searched with fixed-string, case-insensitive matching.

Changed files not searched: none named.

## Located consumers

Kinds are path hints for inspection; each file appears once. Fill every Disposition with `run`, `reused` or `excluded: <reason the effect does not reach its boundary>`.

### Production

| File | Terms matched | First matching line | Disposition |
| --- | --- | --- | --- |
| `docs/cli.rst` | `parameter_count` | 67: `cyclomatic_complexity, token_count, parameter_count, etc. Or an customized field.` |excluded: no execution of or observation of this reader effect |
| `docs/extensions-and-warnings.rst` | `parameter_count` | 87: `Selective forgiveness: Use "#lizard forgives(metric1, metric2)" to forgive only specific metrics (e.g. length, cyclomatic_complexity, parameter_count, nloc, to…` |excluded: no execution of or observation of this reader effect |
| `index.py` | `parameter_count` | 61: `"cyclomatic_complexity", "token_count", "parameter_count")` |run: coordinator owns fresh current-base HTTP/full-suite observation; pending at implementation return |
| `lizard_core/function_info.py` | `parameter_count`, `.parameters`, `full_parameters` | 41: `self.full_parameters = []` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `lizard_core/options.py` | `parameter_count` | 188: `parameter_count, etc. Or an customized field.''',` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `lizard_core/output_scheme.py` | `parameter_count` | 39: `{'caption': " PARAM ", 'value': "parameter_count"},` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `lizard_ext/checkstyleoutput.py` | `.parameters` | 25: `f"{func.token_count} token, {len(func.parameters)} PARAM, {func.length} length",` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `lizard_ext/csvoutput.py` | `.parameters` | 56: `len(source_function.parameters),` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `lizard_ext/htmloutput.py` | `parameter_count`, `full_parameters` | 157: `<th>Parameter count ({{ thresholds["parameter_count"] }})</th>` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `lizard_ext/lizardduplicated_param_list.py` | `.parameters` | 31: `if len(f.parameters) >= DEFAULT_MIN_PARAM_COUNT` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `lizard_languages/__init__.py` | `TypeScriptReader` | 21: `from .typescript import TypeScriptReader` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `lizard_languages/javascript.py` | `TypeScriptReader` | 5: `from .typescript import TypeScriptReader` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `lizard_languages/python_structure.py` | `full_parameters` | 88: `function.full_parameters[-1] += " " + token` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `lizard_languages/tsx.py` | `TypeScriptReader` | 10: `JS_IDENTIFIER, NULLISH_COALESCING, OPTIONAL_ACCESS, TypeScriptReader)` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `lizard_languages/typescript.py` | `TypeScriptReader` | 58: `class TypeScriptReader(CodeReader, CCppCommentsMixin):` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `lizard_languages/vue.py` | `TypeScriptReader` | 6: `from .typescript import TypeScriptReader` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `README.rst` | `parameter_count` | 161: `{'cyclomatic_complexity': 1, 'token_count': 22, 'name': 'main', 'parameter_count': 2, 'nloc': 3, 'long_name': 'main( int ac , const char ** av )', 'start_line'…` |excluded: guidance/history names the concept but does not execute the reader |
| `templates/index.html` | `function-results` | 65: `<div class="table-scroll" tabindex="0" role="region" aria-label="Function metrics"><table><thead><tr><th scope="col">Function</th><th scope="col"><abbr title="…` |run: coordinator owns fresh current-base HTTP/full-suite observation; pending at implementation return |
| `website/static/demo.js` | `parameter_count`, `function-results` | 14: `const rows = document.querySelector("#function-results");` |run: coordinator owns fresh current-base HTTP/full-suite observation; pending at implementation return |

### Test

| File | Terms matched | First matching line | Disposition |
| --- | --- | --- | --- |
| `test/test_analyzer.py` | `parameter_count`, `full_parameters` | 135: `fun.full_parameters = ['a'] * 5  # 5 params` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `test/test_extensions/testCognitiveComplexity.py` | `parameter_count` | 723: `"token_count", "parameter_count", "start_line",` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `test/test_languages/testCAndCPP.py` | `parameter_count`, `.parameters` | 119: `self.assertEqual(0, result[0].parameter_count)` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testFortranProcedureBoundaries.py` | `parameter_count` | 36: `(first.parameter_count, second.parameter_count))` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testGo.py` | `parameter_count`, `full_parameters` | 31: `self.assertEqual(0, result[0].parameter_count)` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testGoDeclarations.py` | `parameter_count` | 14: `self.assertEqual(1, result[0].parameter_count)` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testGoFunctionLiterals.py` | `full_parameters` | 17: `assert result[0].full_parameters == ['n int']` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testGoFunctionTypes.py` | `parameter_count` | 49: `assert result[0].parameter_count == 1` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testJavaScript.py` | `parameter_count`, `TypeScriptReader` | 4: `from lizard_languages import TypeScriptReader` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `test/test_languages/testJavaScriptFunctions.py` | `parameter_count` | 15: `def test_parameter_count(self):` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `test/test_languages/testJavaScriptParameterNames.py` | `parameter_count`, `.parameters` | 22: `assert function.parameters == ['$', '$$', 'a$', 'a$b', '_', '_x',` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `test/test_languages/testKotlin.py` | `parameter_count` | 37: `self.assertEqual(0, result[0].parameter_count)` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testKotlinExpressionBody.py` | `parameter_count` | 21: `self.assertEqual([1, 1, 1], [f.parameter_count for f in result])` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testLua.py` | `parameter_count` | 52: `self.assertEqual(0, result[0].parameter_count)` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testOptionalOperators.py` | `parameter_count` | 104: `assert (function.name, function.parameter_count,` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `test/test_languages/testPHP.py` | `parameter_count` | 97: `def test_parameter_count(self):` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testPLSQL.py` | `parameter_count` | 56: `self.assertEqual(2, result[0].parameter_count)` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testPython.py` | `parameter_count`, `.parameters` | 44: `def test_parameter_count(self):` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testPythonParameters.py` | `parameter_count`, `.parameters` | 7: `def test_parameter_count_with_comma_in_default_value(self):` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testPythonTypeParameters.py` | `parameter_count` | 40: `self.assertEqual(plain.parameter_count, generic.parameter_count)` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testRuby.py` | `parameter_count` | 119: `self.assertEqual(0, result[0].parameter_count)` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testScala.py` | `parameter_count` | 32: `self.assertEqual(0, result[0].parameter_count)` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testSwift.py` | `parameter_count` | 35: `self.assertEqual(0, result[0].parameter_count)` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testTTCN.py` | `parameter_count` | 221: `self.assertEqual(0, result[0].parameter_count)` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testTypeScript.py` | `parameter_count`, `TypeScriptReader` | 4: `from lizard_languages import TypeScriptReader` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `test/test_languages/testTypeScriptMetrics.py` | `parameter_count` | 13: `self.assertEqual(3, functions[0].parameter_count)` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `test/test_languages/testZig.py` | `parameter_count`, `full_parameters` | 25: `self.assertEqual(0, result[0].parameter_count)` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_options.py` | `parameter_count` | 55: `self.assertEqual(123, options.thresholds['parameter_count'])` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `test/test_website.py` | `parameter_count` | 67: `assert function["parameter_count"] == parameters` |run: coordinator owns fresh current-base HTTP/full-suite observation; pending at implementation return |
| `test/testCommentOptions.py` | `parameter_count` | 30: `"void foo(){/* #lizard forgives(length, parameter_count) */}")` |excluded: C++ comment forgiveness fixture cannot enter the JS parameter reader |

### Test support or stand-in

none found

### Journey or feature

none found

### Guidance reader

| File | Terms matched | First matching line | Disposition |
| --- | --- | --- | --- |
| `.planning/slice-plans/004-fabio-malpezzi-pr-reviews/PLAN.md` | `parameter_count`, `TypeScriptReader`, `.parameters`, `full_parameters` | 24: `- JS inherits TypeScriptReader. TypeScriptStates composes function, binding,` |excluded: guidance/history names the concept but does not execute the reader |
| `CHANGELOG.md` | `parameter_count` | 158: `` - Use `#lizard forgives(length, parameter_count)` for multiple metrics `` |excluded: guidance/history names the concept but does not execute the reader |
| `prompt-history.md` | `TypeScriptReader` | 58: `TypeScript is a super set of JavaScript, right? If so, all test in @testJavaScript.py should also pass with a TypeScriptReader from @typescript.py . Please cha…` |excluded: no execution of or observation of this reader effect |
