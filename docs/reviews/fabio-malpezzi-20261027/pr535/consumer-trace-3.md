# Consumer trace

## Effect terms searched

| Term | Origin |
| --- | --- |
| `.parameters` | supplied |
| `full_parameters` | supplied |

## Scope

Whole checkout at `c3ca2c73d3c7`, including uncommitted and untracked files: 1187 tracked and untracked non-ignored files searched with fixed-string, case-insensitive matching.

Changed files not searched: none named.

## Located consumers

Kinds are path hints for inspection; each file appears once. Fill every Disposition with `run`, `reused` or `excluded: <reason the effect does not reach its boundary>`.

### Production

| File | Terms matched | First matching line | Disposition |
| --- | --- | --- | --- |
| `docs/reviews/fabio-malpezzi-20261027/paired.py` | `.parameters` | 33: `function.parameters` |reused: historical executor replay remains pinned to its recorded immutable source/harness; current harness unchanged |
| `docs/reviews/fabio-malpezzi-20261027/pr532/baseline-final-reproduction.log` | `full_parameters` | 7: `{"case": "reported.ts", "expected": [["render", 3], ["round", 2]], "actual": [["render", 1], ["round", 1]], "passed": false, "parameters": [["text"], ["precisi…` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr532/baseline-supplemental.log` | `full_parameters` | 7: `{"case": "php-multiple.php", "expected": [["plain", 2]], "actual": [["plain", 1]], "passed": false, "parameters": [["tail"]], "full_parameters": [["$value $tai…` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr532/corrected-final-reproduction.log` | `full_parameters` | 7: `{"case": "reported.ts", "expected": [["render", 3], ["round", 2]], "actual": [["render", 3], ["round", 2]], "passed": true, "parameters": [["text", "_key", "$e…` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr532/corrected-supplemental.log` | `full_parameters` | 7: `{"case": "php-multiple.php", "expected": [["plain", 2]], "actual": [["plain", 1]], "passed": false, "parameters": [["$tail"]], "full_parameters": [["$value $ta…` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr532/dependency-535-supplemental.log` | `full_parameters` | 7: `{"case": "php-multiple.php", "expected": [["plain", 2]], "actual": [["plain", 1]], "passed": false, "parameters": [["tail"]], "full_parameters": [["$value $tai…` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr532/raw-target-final-reproduction.log` | `full_parameters` | 7: `{"case": "reported.ts", "expected": [["render", 3], ["round", 2]], "actual": [["render", 3], ["round", 2]], "passed": true, "parameters": [["text", "_key", "el…` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr532/raw-target-focused.part1.log` | `.parameters` | 33: `>       assert function.parameters == ['$', '$$', 'a$', 'a$b', '_', '_x',` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr532/raw-target-focused.part2.log` | `.parameters` | 98: `>       assert function.parameters == ['number', '$y', 'tail']` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr532/raw-target-preexisting.log` | `full_parameters` | 7: `{"case": "generics.ts", "expected": [["generic", 3]], "actual": [], "passed": false, "parameters": [], "full_parameters": [], "ccn": []}` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr532/raw-target-red.part1.log` | `.parameters` | 13: `>       assert function.parameters == ['$', '$$', 'a$', 'a$b', '_', '_x',` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr532/raw-target-red.part2.log` | `.parameters` | 78: `>       assert function.parameters == ['number', '$y', 'tail']` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr532/raw-target-supplemental.log` | `full_parameters` | 7: `{"case": "php-multiple.php", "expected": [["plain", 2]], "actual": [["plain", 1]], "passed": false, "parameters": [["tail"]], "full_parameters": [["$value $tai…` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr532/reproduce.py` | `.parameters`, `full_parameters` | 30: `parameters=[f.parameters for f in functions],` |reused: historical executor replay remains pinned to its recorded immutable source/harness; current harness unchanged |
| `docs/reviews/fabio-malpezzi-20261027/pr532/run-proof.py` | `full_parameters` | 42: `'full_parameters', 'ccn')}` |reused: historical executor replay remains pinned to its recorded immutable source/harness; current harness unchanged |
| `docs/reviews/fabio-malpezzi-20261027/pr532/target-final-reproduction.log` | `full_parameters` | 7: `{"case": "reported.ts", "expected": [["render", 3], ["round", 2]], "actual": [["render", 1], ["round", 1]], "passed": false, "parameters": [["text"], ["precisi…` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr532/target-preexisting.log` | `full_parameters` | 7: `{"case": "generics.ts", "expected": [["generic", 3]], "actual": [], "passed": false, "parameters": [], "full_parameters": [], "ccn": []}` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr532/target-red.log` | `.parameters` | 13: `>       assert function.parameters == ['$', '$$', 'a$', 'a$b', '_', '_x',` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr532/target-supplemental.log` | `full_parameters` | 7: `{"case": "php-multiple.php", "expected": [["plain", 2]], "actual": [["plain", 1]], "passed": false, "parameters": [["tail"]], "full_parameters": [["$value $tai…` |excluded: historical evidence snapshot, not a current product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr535/finish-proof.py` | `.parameters`, `full_parameters` | 16: `['node', str(trace), '--checkout', str(REPO), '--term', '.parameters',` |run: literal README commands; bootstrap uses immutable Git reader bytes, never normalized display patches |
| `docs/reviews/fabio-malpezzi-20261027/pr535/reproduce.py` | `.parameters` | 28: `parameters = [f.parameters for f in result.function_list]` |run: literal README commands; bootstrap uses immutable Git reader bytes, never normalized display patches |
| `lizard_core/function_info.py` | `.parameters`, `full_parameters` | 41: `self.full_parameters = []` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `lizard_ext/checkstyleoutput.py` | `.parameters` | 25: `f"{func.token_count} token, {len(func.parameters)} PARAM, {func.length} length",` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `lizard_ext/csvoutput.py` | `.parameters` | 56: `len(source_function.parameters),` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `lizard_ext/htmloutput.py` | `full_parameters` | 188: `{% if func.full_parameters\|length > thresholds["parameter_count"] %}` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `lizard_ext/lizardduplicated_param_list.py` | `.parameters` | 31: `if len(f.parameters) >= DEFAULT_MIN_PARAM_COUNT` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `lizard_languages/python_structure.py` | `full_parameters` | 88: `function.full_parameters[-1] += " " + token` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |

### Test

| File | Terms matched | First matching line | Disposition |
| --- | --- | --- | --- |
| `test/test_analyzer.py` | `full_parameters` | 135: `fun.full_parameters = ['a'] * 5  # 5 params` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `test/test_languages/testCAndCPP.py` | `.parameters` | 128: `self.assertEqual(["bb"], result[0].parameters)` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testGo.py` | `full_parameters` | 100: `self.assertEqual(['n int'], result[1].full_parameters)` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testGoFunctionLiterals.py` | `full_parameters` | 17: `assert result[0].full_parameters == ['n int']` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testJavaScriptParameterNames.py` | `.parameters` | 22: `assert function.parameters == ['$', '$$', 'a$', 'a$b', '_', '_x',` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `test/test_languages/testPython.py` | `.parameters` | 57: `self.assertEqual(['a', 'b'], functions[0].parameters)` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testPythonParameters.py` | `.parameters` | 15: `self.assertEqual(['a', 'b'], functions[0].parameters)` |excluded: another language reader; FunctionInfo/other readers are unchanged |
| `test/test_languages/testTypeScriptThisParameters.py` | `.parameters` | 30: `assert [f.parameters for f in result] == [['x'], [], ['x']]` |run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof |
| `test/test_languages/testZig.py` | `full_parameters` | 101: `self.assertEqual(["n : int"], result[1].full_parameters)` |excluded: another language reader; FunctionInfo/other readers are unchanged |

### Test support or stand-in

none found

### Journey or feature

none found

### Guidance reader

| File | Terms matched | First matching line | Disposition |
| --- | --- | --- | --- |
| `.planning/slice-plans/004-fabio-malpezzi-pr-reviews/532-review.md` | `full_parameters` | 54: `with unchanged full_parameters=['$value $tail']; corrected normalized last name` |excluded: guidance/history names the concept but does not execute the reader |
| `.planning/slice-plans/004-fabio-malpezzi-pr-reviews/CONTEXT.md` | `.parameters`, `full_parameters` | 12: `- FunctionInfo.parameters derives public parameter counts from full_parameters.` |excluded: guidance/history names the concept but does not execute the reader |
| `.planning/slice-plans/004-fabio-malpezzi-pr-reviews/PLAN.md` | `.parameters` | 198: `Reported: slice 6 — "The corrected PR532 changes shared FunctionInfo.parameters, so the prior PR527 CLI -ENS timing does not prove the final combined revision;…` |excluded: guidance/history names the concept but does not execute the reader |
| `docs/reviews/fabio-malpezzi-20261027/pr532/consumer-trace-2.md` | `.parameters`, `full_parameters` | 68: `` \| `lizard_ext/checkstyleoutput.py` \| `parameters` \| 25: `f"{func.token_count} token, {len(func.parameters)} PARAM, {func.length} length",` \| run: extension sui… `` |excluded: proof/trace record itself; no product caller |
| `docs/reviews/fabio-malpezzi-20261027/pr532/review-findings.md` | `.parameters`, `full_parameters` | 117: `All retain full_parameters=['$value $tail']; corrected normalized output retains` |excluded: historical evidence snapshot, not a current product caller |
