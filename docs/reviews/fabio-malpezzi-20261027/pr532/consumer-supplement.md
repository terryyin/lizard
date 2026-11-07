# Supplemental consumer trace

This record supersedes static-record exclusions in consumer-trace-1.md and
consumer-trace-2.md for executable benchmark replay and changed replay guidance.
The installed helper/harness trace below expands actual test helper names and
the literal paired.py consumer. No historical benchmark was rerun.

Actual replay observations are retained in harness-replay.json. The historical
extraction/assert/write statements from pr534/freeze.py ran against an owned
external destination; all eleven recorded harness command paths were checked
byte-for-byte against immutable af27c910, and its --help exited 0.
website-consumer.json preserves the first missing-runtime attempt. Root subsequently
resolved it with current-base-proof.json and website-integrated.log, using the
isolated current-trunk Nix environment with Flask installed.

| Additional changed/current consumer | Disposition and observation |
| --- | --- |
| Root README and PR528/531/533/534 README replay recipes | run: immutable Git retrieval and original hash verification; current source-copy instructions removed |
| PR528/533/534 review-findings.md and planning 533-review.md | reused: original measured hash preserved and current replay source corrected |
| PR532 prepare-proof.py, measure-long.py, measure-ns.py, final-cli.py | run: current harness and frozen-code/corpus hashes; ENS upper +0.46245%, final ordinary 40-pair upper +2.36131%; earlier inconclusive results retained |
| New trunk index.py::analysis_result | run: four actual HTTP requests assert reported names, dollar names and destructuring counts; executed source paths asserted |
| New trunk test/test_website.py::test_analysis_returns_actual_function_metrics | run: complete current-base pytest1751/7 and make; all website assertions included |
| New trunk website/static/demo.js and templates/index.html | excluded: same numeric JSON schema and table rendering; no UI behavior changes |

All located reader/helper tests were run through the full reader/extension
suites; Python/C++/Ruby/PHP name assertions and duplicated-parameter keys also
have the new high-level tests. PHP's existing two-parameter shape remains
count 1 on original baseline, target, raw and corrected snapshots; only its
normalized final name gains the literal '$'. This is retained as an explicit
pre-existing reader defect, not claimed fixed by the single-parameter control.

# Consumer trace

## Effect terms searched

| Term | Origin |
| --- | --- |
| `get_js_function_list` | supplied |
| `get_ts_function_list` | supplied |
| `get_php_function_list` | supplied |
| `get_cpp_fileinfo_with_extension` | supplied |
| `paired.py` | supplied |

## Scope

Whole checkout at `338073069535`, including uncommitted and untracked files: 1089 tracked and untracked non-ignored files searched with fixed-string, case-insensitive matching.

Changed files not searched: none named.

## Located consumers

Kinds are path hints for inspection; each file appears once. Fill every Disposition with `run`, `reused` or `excluded: <reason the effect does not reach its boundary>`.

### Production

| File | Terms matched | First matching line | Disposition |
| --- | --- | --- | --- |
| `docs/reviews/fabio-malpezzi-20261027/527-cli-ns.json` | `paired.py` | 3: `"/Users/terryyin/.cache/lizard-pr-reviews-20261027/paired.py",` | reused: historical observations; literal replay harness bytes verified in harness-replay.json |
| `docs/reviews/fabio-malpezzi-20261027/527-python-ns.json` | `paired.py` | 3: `"/Users/terryyin/.cache/lizard-pr-reviews-20261027/paired.py",` | reused: historical observations; literal replay harness bytes verified in harness-replay.json |
| `docs/reviews/fabio-malpezzi-20261027/controls/527-python-controls.json` | `paired.py` | 3: `"/Users/terryyin/.cache/lizard-pr-reviews-20261027/controls/paired.py",` | reused: historical observations; literal replay harness bytes verified in harness-replay.json |
| `docs/reviews/fabio-malpezzi-20261027/pr528/528-api.json` | `paired.py` | 3: `"/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr528/paired.py",` | reused: historical observations; literal replay harness bytes verified in harness-replay.json |
| `docs/reviews/fabio-malpezzi-20261027/pr528/528-cli.json` | `paired.py` | 3: `"/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr528/paired.py",` | reused: historical observations; literal replay harness bytes verified in harness-replay.json |
| `docs/reviews/fabio-malpezzi-20261027/pr528/proposed-patch.json` | `get_js_function_list` | 17: `"patch": "@@ -2,6 +2,8 @@\n \n from lizard import analyze_file\n \n+from .testJavaScript import get_js_function_list\n+\n \n class TestJavaScriptRegex(unittest…` | excluded: archived original proposal/proof, not a current replay executor |
| `docs/reviews/fabio-malpezzi-20261027/pr531/531-api.json` | `paired.py` | 3: `"/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr531/paired.py",` | reused: historical observations; literal replay harness bytes verified in harness-replay.json |
| `docs/reviews/fabio-malpezzi-20261027/pr531/531-cli.json` | `paired.py` | 3: `"/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr531/paired.py",` | reused: historical observations; literal replay harness bytes verified in harness-replay.json |
| `docs/reviews/fabio-malpezzi-20261027/pr531/proposed-patch.json` | `get_js_function_list` | 13: `"patch": "@@ -511,3 +511,35 @@ def test_arrow_with_block_and_return(self):\n         functions = get_js_function_list(code)\n         self.assertEqual([\"valid…` | excluded: archived original proposal/proof, not a current replay executor |
| `docs/reviews/fabio-malpezzi-20261027/pr532/full-patch.json` | `get_js_function_list`, `get_ts_function_list` | 1: `{"base":"01afe9c228ba2a3dc5253259d8415d2ef47f0b71","head":"d0713bf5b4dcf4bf24453156a3756688627abbc0","patch":"diff --git a/lizard_languages/javascript_function…` | excluded: archived original proposal/proof, not a current replay executor |
| `docs/reviews/fabio-malpezzi-20261027/pr532/github-patch.json` | `get_js_function_list`, `get_ts_function_list` | 1: `{"patches":[{"filename":"lizard_languages/javascript_functions.py","patch":"@@ -3,13 +3,6 @@\n from .javascript_bindings import JavaScriptBindings\n \n \n-# Ty…` | excluded: archived original proposal/proof, not a current replay executor |
| `docs/reviews/fabio-malpezzi-20261027/pr532/github-pr.json` | `get_js_function_list`, `get_ts_function_list` | 1: `{"pull_request":{"url":"https://github.com/terryyin/lizard/pull/532","number":532,"state":"open","merged":false,"mergeable":true,"draft":false,"body":"### The …` | excluded: archived original proposal/proof, not a current replay executor |
| `docs/reviews/fabio-malpezzi-20261027/pr532/measure.py` | `paired.py` | 14: `command = [sys.executable, str(ROOT / corpus / 'paired.py'),` | run: five terminal measured workloads and asserted current harness hash |
| `docs/reviews/fabio-malpezzi-20261027/pr532/prepare-proof.py` | `paired.py` | 26: `shutil.copyfile(REPO / 'docs/reviews/fabio-malpezzi-20261027/paired.py',` | run: five terminal measured workloads and asserted current harness hash |
| `docs/reviews/fabio-malpezzi-20261027/pr533/533-api-terminal.log` | `paired.py` | 21: `{"command": ["/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/paired.py", "--baseline", "/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/target…` | reused: historical observations; literal replay harness bytes verified in harness-replay.json |
| `docs/reviews/fabio-malpezzi-20261027/pr533/533-api.json` | `paired.py` | 3: `"/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/paired.py",` | reused: historical observations; literal replay harness bytes verified in harness-replay.json |
| `docs/reviews/fabio-malpezzi-20261027/pr533/533-cli-terminal.log` | `paired.py` | 21: `{"command": ["/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/paired.py", "--baseline", "/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/target…` | reused: historical observations; literal replay harness bytes verified in harness-replay.json |
| `docs/reviews/fabio-malpezzi-20261027/pr533/533-cli.json` | `paired.py` | 3: `"/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/paired.py",` | reused: historical observations; literal replay harness bytes verified in harness-replay.json |
| `docs/reviews/fabio-malpezzi-20261027/pr533/authored-tests.py` | `get_js_function_list` | 5: `from test.test_languages.testES6 import get_js_function_list` | excluded: archived original proposal/proof, not a current replay executor |
| `docs/reviews/fabio-malpezzi-20261027/pr533/proposed-patch.json` | `get_js_function_list` | 1: `{"base":"01afe9c228ba2a3dc5253259d8415d2ef47f0b71","head":"15423852c04aed302f684395fe9522251d88a2f2","diff":"diff --git a/lizard_languages/javascript_functions…` | excluded: archived original proposal/proof, not a current replay executor |
| `docs/reviews/fabio-malpezzi-20261027/pr533/target-authored.log` | `get_js_function_list` | 150: `with_semicolon = get_js_function_list(` | excluded: archived original proposal/proof, not a current replay executor |
| `docs/reviews/fabio-malpezzi-20261027/pr534/534-api-terminal.log` | `paired.py` | 21: `{"command": ["/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/paired.py", "--baseline", "/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/target…` | reused: historical observations; literal replay harness bytes verified in harness-replay.json |
| `docs/reviews/fabio-malpezzi-20261027/pr534/534-api.json` | `paired.py` | 3: `"/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/paired.py",` | reused: historical observations; literal replay harness bytes verified in harness-replay.json |
| `docs/reviews/fabio-malpezzi-20261027/pr534/534-cli-terminal.log` | `paired.py` | 21: `{"command": ["/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/paired.py", "--baseline", "/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/target…` | reused: historical observations; literal replay harness bytes verified in harness-replay.json |
| `docs/reviews/fabio-malpezzi-20261027/pr534/534-cli.json` | `paired.py` | 3: `"/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/paired.py",` | reused: historical observations; literal replay harness bytes verified in harness-replay.json |
| `docs/reviews/fabio-malpezzi-20261027/pr534/freeze.py` | `paired.py` | 114: `'docs/reviews/fabio-malpezzi-20261027/paired.py'], cwd=HERE)` | run: actual extraction/assert/write fragment and immutable hash (harness-replay.json) |
| `docs/reviews/fabio-malpezzi-20261027/pr534/measure.py` | `paired.py` | 11: `command = [sys.executable, str(CACHE / 'paired.py'),` | reused: historical observations; literal replay harness bytes verified in harness-replay.json |
| `docs/reviews/fabio-malpezzi-20261027/pr534/proposed-patch.json` | `get_js_function_list` | 1: `{"base": "01afe9c228ba2a3dc5253259d8415d2ef47f0b71", "head": "f4074ae860c7999cc37dd081a6f7ebfa777adc83", "patch": "diff --git a/lizard_languages/javascript_fun…` | excluded: archived original proposal/proof, not a current replay executor |

### Test

| File | Terms matched | First matching line | Disposition |
| --- | --- | --- | --- |
| `test/test_extensions/testDuplicate.py` | `get_cpp_fileinfo_with_extension` | 3: `from ..testHelpers import get_cpp_fileinfo_with_extension` | run: complete reader/extension suites; coordinator full 1721/7 |
| `test/test_extensions/testDuplicatedParamList.py` | `get_cpp_fileinfo_with_extension` | 3: `from ..testHelpers import get_cpp_fileinfo_with_extension` | run: complete reader/extension suites; coordinator full 1721/7 |
| `test/test_languages/testES6.py` | `get_js_function_list` | 6: `def get_js_function_list(source_code):` | run: complete reader/extension suites; coordinator full 1721/7 |
| `test/test_languages/testJavaScript.py` | `get_js_function_list` | 7: `def get_js_function_list(source_code):` | run: complete reader/extension suites; coordinator full 1721/7 |
| `test/test_languages/testJavaScriptFalsePositives.py` | `get_js_function_list` | 3: `from .testJavaScript import get_js_function_list` | run: complete reader/extension suites; coordinator full 1721/7 |
| `test/test_languages/testJavaScriptFunctions.py` | `get_js_function_list` | 3: `from .testJavaScript import get_js_function_list` | run: complete reader/extension suites; coordinator full 1721/7 |
| `test/test_languages/testJavaScriptMembers.py` | `get_js_function_list` | 3: `from .testJavaScript import get_js_function_list` | run: complete reader/extension suites; coordinator full 1721/7 |
| `test/test_languages/testJavaScriptWidgets.py` | `get_js_function_list` | 3: `from .testJavaScript import get_js_function_list` | run: complete reader/extension suites; coordinator full 1721/7 |
| `test/test_languages/testPHP.py` | `get_php_function_list` | 4: `get_php_function_list,` | run: complete reader/extension suites; coordinator full 1721/7 |
| `test/test_languages/testPHPFunctionRecognition.py` | `get_php_function_list` | 2: `from .php_helpers import get_php_function_list` | run: complete reader/extension suites; coordinator full 1721/7 |
| `test/test_languages/testPHPModernFeatures.py` | `get_php_function_list` | 2: `from .php_helpers import get_php_function_list` | run: complete reader/extension suites; coordinator full 1721/7 |
| `test/test_languages/testTypeScript.py` | `get_ts_function_list` | 7: `def get_ts_function_list(source_code):` | run: complete reader/extension suites; coordinator full 1721/7 |
| `test/test_languages/testTypeScriptArrowFields.py` | `get_ts_function_list` | 3: `from .testTypeScript import get_ts_function_list` | run: complete reader/extension suites; coordinator full 1721/7 |
| `test/test_languages/testTypeScriptDeclarations.py` | `get_ts_function_list` | 3: `from .testTypeScript import get_ts_function_list` | run: complete reader/extension suites; coordinator full 1721/7 |
| `test/test_languages/testTypeScriptFalsePositives.py` | `get_ts_function_list` | 3: `from .testTypeScript import get_ts_function_list` | run: complete reader/extension suites; coordinator full 1721/7 |
| `test/test_languages/testTypeScriptFields.py` | `get_ts_function_list` | 3: `from .testTypeScript import get_ts_function_list` | run: complete reader/extension suites; coordinator full 1721/7 |
| `test/test_languages/testTypeScriptFunctions.py` | `get_ts_function_list` | 3: `from .testTypeScript import get_ts_function_list` | run: complete reader/extension suites; coordinator full 1721/7 |
| `test/test_languages/testTypeScriptMembers.py` | `get_ts_function_list` | 3: `from .testTypeScript import get_ts_function_list` | run: complete reader/extension suites; coordinator full 1721/7 |
| `test/test_languages/testTypeScriptMetrics.py` | `get_ts_function_list` | 3: `from .testTypeScript import get_ts_function_list` | run: complete reader/extension suites; coordinator full 1721/7 |
| `test/test_languages/testTypeScriptWidgets.py` | `get_ts_function_list` | 3: `from .testTypeScript import get_ts_function_list` | run: complete reader/extension suites; coordinator full 1721/7 |
| `test/testHelpers.py` | `get_cpp_fileinfo_with_extension` | 18: `def get_cpp_fileinfo_with_extension(source_code, extension):` | run: complete reader/extension suites; coordinator full 1721/7 |

### Test support or stand-in

| File | Terms matched | First matching line | Disposition |
| --- | --- | --- | --- |
| `test/test_languages/php_helpers.py` | `get_php_function_list` | 5: `def get_php_function_list(source_code):` | run: complete reader/extension suites; coordinator full 1721/7 |

### Journey or feature

none found

### Guidance reader

| File | Terms matched | First matching line | Disposition |
| --- | --- | --- | --- |
| `.planning/slice-plans/004-fabio-malpezzi-pr-reviews/527-review.md` | `paired.py` | 76: `` `nix develop -c python /Users/terryyin/.cache/lizard-pr-reviews-20261027/paired.py --baseline /Users/terryyin/.cache/lizard-pr-reviews-20261027/baseline --cand… `` | reused: historical observations; literal replay harness bytes verified in harness-replay.json |
| `.planning/slice-plans/004-fabio-malpezzi-pr-reviews/533-review.md` | `paired.py` | 52: `The literal central paired.py harness is unchanged; API analyzes only related` | reused: historical observations; literal replay harness bytes verified in harness-replay.json |
| `docs/reviews/fabio-malpezzi-20261027/pr528/review-findings.md` | `paired.py` | 89: `` `nix develop -c python /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr528/paired.py --baseline /Users/terryyin/.cache/lizard-pr-reviews-20261027/baseline … `` | reused: historical observations; literal replay harness bytes verified in harness-replay.json |
| `docs/reviews/fabio-malpezzi-20261027/pr531/review-findings.md` | `paired.py` | 160: `PIP_QUIET=1 nix develop -c python /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr531/paired.py --baseline /Users/terryyin/.cache/lizard-pr-reviews-2026102…` | reused: historical observations; literal replay harness bytes verified in harness-replay.json |
| `docs/reviews/fabio-malpezzi-20261027/pr532/consumer-trace-1.md` | `paired.py` | 11: `` \| `docs/reviews/fabio-malpezzi-20261027/paired.py` \| changed path \| `` | excluded: trace self-reference; no executable behavior |
| `docs/reviews/fabio-malpezzi-20261027/pr532/consumer-trace-2.md` | `paired.py` | 15: `` \| `docs/reviews/fabio-malpezzi-20261027/pr534/534-api-terminal.log` \| `paired.py` \| 21: `{"command": ["/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/… `` | excluded: trace self-reference; no executable behavior |
| `docs/reviews/fabio-malpezzi-20261027/pr533/README.md` | `paired.py` | 27: `manifest.json beside the external corpus and identical paired.py. The original` | reused: historical observations; literal replay harness bytes verified in harness-replay.json |
| `docs/reviews/fabio-malpezzi-20261027/pr533/review-findings.md` | `paired.py` | 114: `The originally measured paired.py, recoverable with the parent README's immutable recipe, has SHA256` | reused: historical observations; literal replay harness bytes verified in harness-replay.json |
| `docs/reviews/fabio-malpezzi-20261027/README.md` | `paired.py` | 19: `git show af27c910a6a0bff51d8358fde0c37fa9cb059d1a:docs/reviews/fabio-malpezzi-20261027/paired.py > /absolute/historical-root/paired.py` | reused: historical observations; literal replay harness bytes verified in harness-replay.json |
