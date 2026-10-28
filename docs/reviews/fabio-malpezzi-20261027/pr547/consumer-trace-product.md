# Consumer trace

## Effect terms searched

| Term | Origin |
| --- | --- |
| `lizard_languages/tsx.py` | changed path |
| `tsx.py` | basename of `lizard_languages/tsx.py` |
| `lizard_languages/typescript.py` | changed path |
| `typescript.py` | basename of `lizard_languages/typescript.py` |
| `test/test_languages/testOptionalOperators.py` | changed path |
| `testOptionalOperators.py` | basename of `test/test_languages/testOptionalOperators.py` |
| `OPTIONAL_NAME` | supplied |
| `generate_tokens` | supplied |
| `TypeScriptReader` | supplied |
| `TSXReader` | supplied |

## Scope

Whole checkout at `989118d1400e`, including uncommitted and untracked files: 1616 tracked and untracked non-ignored files searched with fixed-string, case-insensitive matching.

Changed files not searched: `lizard_languages/tsx.py`, `lizard_languages/typescript.py`, `test/test_languages/testOptionalOperators.py`.

## Located consumers

Kinds are path hints for inspection; each file appears once. Fill every Disposition with `run`, `reused` or `excluded: <reason the effect does not reach its boundary>`.

### Production

| File | Terms matched | First matching line | Disposition |
| --- | --- | --- | --- |
| `docs/reviews/fabio-malpezzi-20261027/controls/manifest.json` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 97: `"lizard_languages/tsx.py": "2a021159330f0c28a2764893db70e4bb345acb4e3f1f253de5adc415dbe3c9b9",` | |
| `docs/reviews/fabio-malpezzi-20261027/manifest.json` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 97: `"lizard_languages/tsx.py": "2a021159330f0c28a2764893db70e4bb345acb4e3f1f253de5adc415dbe3c9b9",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr528/528-profile-baseline.json` | `generate_tokens` | 63: `"function": "generate_tokens_with_regex",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr528/528-profile-candidate.json` | `generate_tokens` | 71: `"function": "generate_tokens_with_regex",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr528/manifest.json` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 97: `"lizard_languages/tsx.py": "2a021159330f0c28a2764893db70e4bb345acb4e3f1f253de5adc415dbe3c9b9",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr531/531-focused-candidate-correct-imports.log` | `tsx.py` | 1: `command=['/Users/terryyin/git/lizard/.worktrees/review-correct-and-merge-fabiomalpezzi-s-open-pr/.venv/bin/python', '-c', "import sys,pathlib\nroot=pathlib.Pat…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr531/531-focused-candidate-isolated.log` | `tsx.py`, `typescript.py` | 3: `command=['/Users/terryyin/git/lizard/.worktrees/review-correct-and-merge-fabiomalpezzi-s-open-pr/.venv/bin/python', '-c', "import sys,pathlib\nroot=pathlib.Pat…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr531/531-focused-candidate.log` | `tsx.py`, `typescript.py` | 1: `command=['/Users/terryyin/git/lizard/.worktrees/review-correct-and-merge-fabiomalpezzi-s-open-pr/.venv/bin/python', '-c', "import sys,pathlib,pytest; root=path…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr531/531-profile-baseline.json` | `generate_tokens` | 73: `"function": "generate_tokens_with_regex",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr531/531-profile-candidate.json` | `generate_tokens` | 73: `"function": "generate_tokens_with_regex",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr531/manifest.json` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 99: `"lizard_languages/tsx.py": "2a021159330f0c28a2764893db70e4bb345acb4e3f1f253de5adc415dbe3c9b9",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/affected-manifest.json` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 111: `"lizard_languages/tsx.py": "2a021159330f0c28a2764893db70e4bb345acb4e3f1f253de5adc415dbe3c9b9",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/baseline-final-reproduction.log` | `typescript.py` | 6: `{"module": "test.test_languages.testTypeScript", "path": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/baseline/test/test_languages/testTypeScript.py"}` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/baseline-supplemental.log` | `typescript.py` | 6: `{"module": "test.test_languages.testTypeScript", "path": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/baseline/test/test_languages/testTypeScript.py"}` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/consumers-manifest.json` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 116: `"lizard_languages/tsx.py": "2a021159330f0c28a2764893db70e4bb345acb4e3f1f253de5adc415dbe3c9b9",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/corrected-final-reproduction.log` | `typescript.py` | 6: `{"module": "test.test_languages.testTypeScript", "path": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr532/corrected/test/test_languages/testTypeScript.…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/corrected-supplemental.log` | `typescript.py` | 6: `{"module": "test.test_languages.testTypeScript", "path": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr532/corrected/test/test_languages/testTypeScript.…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/current-base-make.log.part1` | `tsx.py`, `typescript.py`, `test/test_languages/testOptionalOperators.py`, `testOptionalOperators.py` | 83: `test/test_languages/testOptionalOperators.py ........................... [ 60%]` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/current-base-make.log.part2` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 9: `lizard_languages/tsx.py                           109      5    95%   94, 125, 149, 168-169` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/current-base-make.log.part3` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 12: `lizard_languages/tsx.py:37:12: R1737: [1;3;35mUse 'yield from' directly instead of yielding each element one by one[0m ([1;3;35muse-yield-from[0m)` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/current-base-proof.json` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 9: `"lizard_languages/typescript.py",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/current-base-pytest.log` | `tsx.py`, `typescript.py`, `test/test_languages/testOptionalOperators.py`, `testOptionalOperators.py` | 82: `test/test_languages/testOptionalOperators.py ........................... [ 60%]` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/dependency-535-supplemental.log` | `typescript.py` | 6: `{"module": "test.test_languages.testTypeScript", "path": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr532/dependency-535/test/test_languages/testTypeSc…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/final-cli.py` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 15: `'lizard_languages/typescript.py', 'lizard_languages/tsx.py']` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/finish-proof.py` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 35: `'lizard_languages/javascript_functions.py', 'lizard_languages/typescript.py',` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/observe-website.py` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 52: `'lizard_languages/typescript.py', 'lizard_languages/tsx.py'):` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/prepare-proof.py` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 15: `'lizard_languages/typescript.py', 'lizard_languages/tsx.py']` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/raw-target-final-reproduction.log` | `typescript.py` | 6: `{"module": "test.test_languages.testTypeScript", "path": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr532/raw-target/test/test_languages/testTypeScript…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/raw-target-preexisting.log` | `typescript.py` | 6: `{"module": "test.test_languages.testTypeScript", "path": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr532/raw-target/test/test_languages/testTypeScript…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/raw-target-supplemental.log` | `typescript.py` | 6: `{"module": "test.test_languages.testTypeScript", "path": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr532/raw-target/test/test_languages/testTypeScript…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/snapshot-hashes.json` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 6: `"lizard_languages/typescript.py": "e4d0b99eb9b88a1cddfd413c7cff2377e0bb66e3085ee8391f7a21ed1bbe08ab",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/target-final-reproduction.log` | `typescript.py` | 6: `{"module": "test.test_languages.testTypeScript", "path": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr532/target/test/test_languages/testTypeScript.py"}` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/target-preexisting.log` | `typescript.py` | 6: `{"module": "test.test_languages.testTypeScript", "path": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr532/target/test/test_languages/testTypeScript.py"}` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/target-supplemental.log` | `typescript.py` | 6: `{"module": "test.test_languages.testTypeScript", "path": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr532/target/test/test_languages/testTypeScript.py"}` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/validate-current-base.py` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 18: `'lizard_languages/typescript.py', 'lizard_languages/tsx.py']` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/website-consumer.json` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 9: `"lizard_languages/typescript.py": "74146767409186ca9cadcc0175f1e72ab58e089c10999553fee4d0fe11f9784f",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr533/manifest.json` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 1: `{"revision": "2e21e404c1ab9f4e0b49acc80816e816629d0670", "target": "7ac449f59b3f155fd159ce9aad6794a422045632", "head": "15423852c04aed302f684395fe9522251d88a2f…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr533/raw-profile.json` | `generate_tokens` | 53: `"function": "generate_tokens_with_regex",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr533/raw-profile.log` | `generate_tokens` | 1: `{"checkout": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/raw", "loops": 5000, "observed_metrics": [["f", 1, 4, 2, 4, 12], ["after", 5, 5, 1, 1, 7]…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr533/reproduce.py` | `tsx.py` | 35: `'testTSX.py', 'testVue.py')` | |
| `docs/reviews/fabio-malpezzi-20261027/pr533/target-profile.json` | `generate_tokens` | 45: `"function": "generate_tokens_with_regex",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr533/target-profile.log` | `generate_tokens` | 1: `{"checkout": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/target", "loops": 5000, "observed_metrics": [["f", 1, 4, 2, 4, 12], ["after", 5, 5, 1, 1,…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/affected-manifest.json` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 124: `"lizard_languages/tsx.py": "2a021159330f0c28a2764893db70e4bb345acb4e3f1f253de5adc415dbe3c9b9",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/consumers-manifest.json` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 129: `"lizard_languages/tsx.py": "2a021159330f0c28a2764893db70e4bb345acb4e3f1f253de5adc415dbe3c9b9",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/current-base-make.log.part1` | `tsx.py`, `typescript.py`, `test/test_languages/testOptionalOperators.py`, `testOptionalOperators.py` | 83: `test/test_languages/testOptionalOperators.py ........................... [ 59%]` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/current-base-make.log.part2` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 11: `lizard_languages/tsx.py                           109      5    95%   94, 125, 149, 168-169` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/current-base-make.log.part3` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 14: `lizard_languages/tsx.py:37:12: R1737: [1;3;35mUse 'yield from' directly instead of yielding each element one by one[0m ([1;3;35muse-yield-from[0m)` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/current-base-pytest.log` | `tsx.py`, `typescript.py`, `test/test_languages/testOptionalOperators.py`, `testOptionalOperators.py` | 82: `test/test_languages/testOptionalOperators.py ........................... [ 59%]` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/finish-proof.py` | `test/test_languages/testOptionalOperators.py`, `testOptionalOperators.py`, `TypeScriptReader` | 19: `'--term', 'parameter_count', '--term', 'TypeScriptReader',` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/focused-command.json` | `tsx.py`, `typescript.py` | 14: `"test/test_languages/testTSX.py",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/focused.py` | `tsx.py`, `TypeScriptReader`, `TSXReader` | 13: `['--term', 'TypeScriptReader', '--term', 'TSXReader',` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/snapshot-hashes.json` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 1: `[{"path":"lizard.py","target":"fc83d74b7d74e6b579d81808451e89661c26d599624c5acc4a41614a46253c71","corrected":"fc83d74b7d74e6b579d81808451e89661c26d599624c5acc4…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/adapted-profile.log` | `lizard_languages/typescript.py`, `typescript.py`, `generate_tokens` | 19: `123500    0.033    0.000    0.156    0.000 /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr536/adapted/lizard_languages/js_style_regex_expression.py:16(gen…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/consumer-trace-1.md.part1` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 63: `` \| `docs/reviews/fabio-malpezzi-20261027/pr532/current-base-make.log.part3` \| `lizard_languages/javascript_bindings.py`, `javascript_bindings.py`, `lizard_langu… `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/consumer-trace-1.md.part2` | `typescript.py`, `test/test_languages/testOptionalOperators.py`, `testOptionalOperators.py`, `TypeScriptReader` | 2: `` \| `test/test_languages/testOptionalOperators.py` \| `parameter_count` \| 104: `assert (function.name, function.parameter_count,` \|run: snapshot reader groups, su… `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/finish-proof.py` | `TypeScriptReader` | 22: `'--term', 'TypeScriptReader', '--term', 'function-results'],` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/manifest.json` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 1: `{"affected/annotations.ts": "93f5227836b388139b742a6ded0234207d1f3020f7bd302a4e107cbf99155386", "affected/arrow-abandoned.js": "56a151a284b6035d556f04483910b8f…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/parent-profile.log` | `lizard_languages/typescript.py`, `typescript.py`, `generate_tokens` | 19: `123500    0.033    0.000    0.151    0.000 /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr536/parent/lizard_languages/js_style_regex_expression.py:16(gene…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/raw-profile.log` | `lizard_languages/typescript.py`, `typescript.py`, `generate_tokens` | 19: `123500    0.033    0.000    0.156    0.000 /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr536/raw/lizard_languages/js_style_regex_expression.py:16(generat…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/run-proof.py` | `tsx.py`, `testOptionalOperators.py` | 23: `for name in ('testES6.py', 'testTSX.py', 'testOptionalOperators.py')]` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/source-hashes.json` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 1: `{"target": {"lizard_core/__init__.py": "345df9bf6abd477b82b7546d212473cf78aa1a15450be0f75ba32603206d066c", "lizard_core/analysis.py": "4e6c15d0b00aff70b5264d40…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/target-profile.log` | `lizard_languages/typescript.py`, `typescript.py`, `generate_tokens` | 19: `123500    0.032    0.000    0.148    0.000 /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr536/target/lizard_languages/js_style_regex_expression.py:16(gene…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/adapted.diff` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `generate_tokens`, `TypeScriptReader`, `TSXReader` | 20: `diff --git a/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr544/target/lizard_languages/typescript.py b/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/bootstrap.log` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 6: `{"target": "aa5f40638b23aa54d4724c9429c1ead2183013fd", "head": "3ac7e4eaf61e898836fded88ab715b37f9fedbbf", "baseline": "2e21e404c1ab9f4e0b49acc80816e816629d067…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/bootstrap.py` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 19: `'lizard_languages/typescript.py',` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/execution-summary.json` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 17: `"lizard_languages/typescript.py",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/focused-command.json` | `tsx.py` | 2: `"command": "nix develop -c python -m pytest test/test_languages/testJavaScript*.py test/test_languages/testTypeScript*.py test/test_languages/testES6.py test/t…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/full-pytest.log` | `tsx.py`, `typescript.py`, `test/test_languages/testOptionalOperators.py`, `testOptionalOperators.py`, `TypeScriptReader` | 98: `test/test_languages/testOptionalOperators.py ........................... [ 61%]` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/literal-git-patches.json` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `generate_tokens`, `TypeScriptReader`, `TSXReader` | 1: `{"raw": {"revisions": ["3ac7e4eaf61e898836fded88ab715b37f9fedbbf^", "3ac7e4eaf61e898836fded88ab715b37f9fedbbf"], "patch": "diff --git a/lizard_languages/javasc…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/make.log` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testOptionalOperators.py`, `testOptionalOperators.py`, `TypeScriptReader` | 99: `test/test_languages/testOptionalOperators.py ........................... [ 61%]` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/manifest.json` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 1: `{"affected/b-short.js": "a73d2a3ef4629b77043d7326ba01058059415c808120452315ecb220749eb07d", "affected/b-story.js": "faf4393a26bc51a330b562c5068695e08cf1fe2b825…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/measure.py` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 40: `'lizard_languages/typescript.py',` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/metadata.json` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 1: `` {"baseRefOid":"01afe9c228ba2a3dc5253259d8415d2ef47f0b71","body":"### The problem\n\nThe common tokenizer continues a `//` comment on the next line when the lin… `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/raw.diff` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `generate_tokens`, `TypeScriptReader`, `TSXReader` | 20: `diff --git a/lizard_languages/tsx.py b/lizard_languages/tsx.py` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/reproduction.json` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 7: `"typescript": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr544/target/lizard_languages/typescript.py",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/snapshot-hashes.json` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 9: `"path": "lizard_languages/tsx.py",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/source-hashes.json` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 1: `{"target": {"lizard_core/__init__.py": "345df9bf6abd477b82b7546d212473cf78aa1a15450be0f75ba32603206d066c", "lizard_core/analysis.py": "4e6c15d0b00aff70b5264d40…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr545/adapted.diff` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `generate_tokens`, `TypeScriptReader`, `TSXReader` | 51: `diff --git a/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr545/target/lizard_languages/typescript.py b/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr545/bootstrap-summary.json` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 7: `"lizard_languages/typescript.py",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr545/bootstrap.py` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 19: `'lizard_languages/typescript.py',` | |
| `docs/reviews/fabio-malpezzi-20261027/pr545/focused-command.json` | `tsx.py` | 8: `"test/test_languages/testTSX.py",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr545/focused.log` | `tsx.py`, `typescript.py`, `TypeScriptReader` | 30: `test/test_languages/testTypeScript.py ....................               [ 60%]` | |
| `docs/reviews/fabio-malpezzi-20261027/pr545/literal-git-patches.json` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `generate_tokens`, `TypeScriptReader`, `TSXReader` | 1: `{"raw": {"revisions": ["baa95a4df8278421bb3d3a6487516030cc03a979^", "baa95a4df8278421bb3d3a6487516030cc03a979"], "patch": "diff --git a/lizard_languages/javasc…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr545/manifest.json` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 1: `{"affected/abc.js": "56e579ea0676c6bfa16b9004831dd7d75ae14bddd093bd034612ccfedbf3d762", "affected/abc.jsx": "56e579ea0676c6bfa16b9004831dd7d75ae14bddd093bd0346…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr545/measure.py` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 49: `'lizard_languages/typescript.py',` | |
| `docs/reviews/fabio-malpezzi-20261027/pr545/metadata.json` | `lizard_languages/typescript.py`, `typescript.py` | 1: `{"baseRefOid":"01afe9c228ba2a3dc5253259d8415d2ef47f0b71","body":"### The problem\n\nThe expression of a template interpolation is kept as one token for the\nst…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr545/raw.diff` | `lizard_languages/typescript.py`, `typescript.py`, `generate_tokens`, `TypeScriptReader` | 51: `diff --git a/lizard_languages/typescript.py b/lizard_languages/typescript.py` | |
| `docs/reviews/fabio-malpezzi-20261027/pr545/reproduction.json` | `lizard_languages/typescript.py`, `typescript.py` | 7: `"typescript": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr545/target/lizard_languages/typescript.py"` | |
| `docs/reviews/fabio-malpezzi-20261027/pr545/snapshot-hashes.json` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 9: `"path": "lizard_languages/tsx.py",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr545/source-hashes.json` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 1: `{"target": {"lizard_core/__init__.py": "345df9bf6abd477b82b7546d212473cf78aa1a15450be0f75ba32603206d066c", "lizard_core/analysis.py": "4e6c15d0b00aff70b5264d40…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr546/adapted.diff` | `lizard_languages/typescript.py`, `typescript.py`, `TypeScriptReader` | 2: `diff --git a/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr546/target/lizard_languages/typescript.py b/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr546/bootstrap-summary.json` | `lizard_languages/typescript.py`, `typescript.py` | 6: `"lizard_languages/typescript.py",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr546/bootstrap.log` | `lizard_languages/typescript.py`, `typescript.py` | 11: `"lizard_languages/typescript.py",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr546/bootstrap.py` | `lizard_languages/typescript.py`, `typescript.py` | 18: `'lizard_languages/typescript.py',` | |
| `docs/reviews/fabio-malpezzi-20261027/pr546/execution-summary.json` | `lizard_languages/typescript.py`, `typescript.py`, `TypeScriptReader` | 12: `"adaptation": "TypeScriptReader.process_token undoes keyword members; fast path allows TypeScriptReader.process_token",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr546/focused-command.json` | `tsx.py` | 2: `"command": "nix develop -c python -m pytest test/test_languages/testJavaScript*.py test/test_languages/testTypeScript*.py test/test_languages/testES6.py test/t…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr546/literal-git-patches.json` | `lizard_languages/typescript.py`, `typescript.py`, `TypeScriptReader` | 1: `{"raw": {"revisions": ["a671632141127fca9b2785fdbb0c675d45ae9042^", "a671632141127fca9b2785fdbb0c675d45ae9042"], "patch": "diff --git a/lizard_languages/typesc…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr546/make.log` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testOptionalOperators.py`, `testOptionalOperators.py`, `TypeScriptReader` | 99: `test/test_languages/testOptionalOperators.py ........................... [ 61%]` | |
| `docs/reviews/fabio-malpezzi-20261027/pr546/manifest.json` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 1: `{"affected/b-story.js": "faf4393a26bc51a330b562c5068695e08cf1fe2b82503d1c094ee033c84fa50a", "affected/genuine-all.js": "83ca72e4726df5645621706ea22096693d246da…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr546/measure.py` | `lizard_languages/typescript.py`, `typescript.py` | 48: `(REPO / 'lizard_languages/typescript.py').read_bytes()` | |
| `docs/reviews/fabio-malpezzi-20261027/pr546/measurement-decision.json` | `typescript.py`, `TypeScriptReader` | 7: `"rationale": "Expanded affected corpus with prior pr544/pr545/pr535/pr536 inputs plus bulk controls and replicas (learning from #544 tiny-corpus inflation and …` | |
| `docs/reviews/fabio-malpezzi-20261027/pr546/raw.diff` | `lizard_languages/typescript.py`, `typescript.py`, `TypeScriptReader` | 2: `diff --git a/lizard_languages/typescript.py b/lizard_languages/typescript.py` | |
| `docs/reviews/fabio-malpezzi-20261027/pr546/reproduction.json` | `lizard_languages/typescript.py`, `typescript.py` | 6: `"typescript": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr546/target/lizard_languages/typescript.py"` | |
| `docs/reviews/fabio-malpezzi-20261027/pr546/snapshot-hashes.json` | `lizard_languages/typescript.py`, `typescript.py` | 3: `"path": "lizard_languages/typescript.py",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr546/source-hashes.json` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 1: `{"target": {"lizard_core/__init__.py": "345df9bf6abd477b82b7546d212473cf78aa1a15450be0f75ba32603206d066c", "lizard_core/analysis.py": "4e6c15d0b00aff70b5264d40…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr547/adapted.diff` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testOptionalOperators.py`, `testOptionalOperators.py`, `OPTIONAL_NAME`, `TypeScriptReader`, `TSXReader` | 2: `diff --git a/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr547/target/lizard_languages/typescript.py b/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr547/bootstrap-summary.json` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testOptionalOperators.py`, `testOptionalOperators.py` | 6: `"lizard_languages/typescript.py",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr547/bootstrap.log` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testOptionalOperators.py`, `testOptionalOperators.py` | 11: `"lizard_languages/typescript.py",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr547/bootstrap.py` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testOptionalOperators.py`, `testOptionalOperators.py` | 18: `'lizard_languages/typescript.py',` | |
| `docs/reviews/fabio-malpezzi-20261027/pr547/literal-git-patches.json` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testOptionalOperators.py`, `testOptionalOperators.py`, `OPTIONAL_NAME`, `generate_tokens`, `TypeScriptReader`, `TSXReader` | 1: `{"raw": {"revisions": ["2766557096daac3f8e55657e9c48ba685157635d^", "2766557096daac3f8e55657e9c48ba685157635d"], "patch": "diff --git a/lizard_languages/tsx.py…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr547/manifest.json` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 1: `{"affected/b-story.js": "faf4393a26bc51a330b562c5068695e08cf1fe2b82503d1c094ee033c84fa50a", "affected/load.js": "4e3d6d63a14aaa4fd070c381e8f84a67775ff7534906e4…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr547/measure.py` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 48: `(REPO / 'lizard_languages/typescript.py').read_bytes()` | |
| `docs/reviews/fabio-malpezzi-20261027/pr547/measurement-decision.json` | `OPTIONAL_NAME`, `generate_tokens` | 7: `"rationale": "Expanded affected corpus with prior pr546/pr544/pr545/pr535/pr536 inputs plus bulk controls and replicas (learning from #544 tiny-corpus inflatio…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr547/metadata.json` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testOptionalOperators.py`, `testOptionalOperators.py` | 1: `{"author":{"id":"U_kgDOBhT8FA","is_bot":false,"login":"FabioMalpezzi","name":""},"baseRefName":"master","baseRefOid":"01afe9c228ba2a3dc5253259d8415d2ef47f0b71"…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr547/raw.diff` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testOptionalOperators.py`, `testOptionalOperators.py`, `OPTIONAL_NAME`, `generate_tokens`, `TypeScriptReader`, `TSXReader` | 2: `diff --git a/lizard_languages/tsx.py b/lizard_languages/tsx.py` | |
| `docs/reviews/fabio-malpezzi-20261027/pr547/reproduction.json` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 6: `"typescript": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr547/target/lizard_languages/typescript.py",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr547/source-hashes.json` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 1: `{"target": {"lizard_core/__init__.py": "345df9bf6abd477b82b7546d212473cf78aa1a15450be0f75ba32603206d066c", "lizard_core/analysis.py": "4e6c15d0b00aff70b5264d40…` | |
| `lizard_core/file_analyzer.py` | `generate_tokens` | 97: `tokens = reader.generate_tokens(code)` | |
| `lizard_languages/__init__.py` | `TypeScriptReader`, `TSXReader` | 21: `from .typescript import TypeScriptReader` | |
| `lizard_languages/clike.py` | `generate_tokens` | 12: `# See #451, #478; subgroup-free per CodeReader.generate_tokens.` | |
| `lizard_languages/code_reader.py` | `generate_tokens` | 136: `def generate_tokens(source_code, addition='', token_class=None):` | |
| `lizard_languages/csharp.py` | `generate_tokens` | 28: `def generate_tokens(source_code, addition='', token_class=None):` | |
| `lizard_languages/erlang.py` | `generate_tokens` | 36: `def generate_tokens(source_code, addition='', token_class=None):` | |
| `lizard_languages/fortran.py` | `generate_tokens` | 36: `def generate_tokens(source_code, addition='', token_class=None):` | |
| `lizard_languages/go.py` | `generate_tokens` | 24: `def generate_tokens(source_code, addition='', token_class=None):` | |
| `lizard_languages/javascript.py` | `TypeScriptReader` | 5: `from .typescript import TypeScriptReader` | |
| `lizard_languages/js_style_regex_expression.py` | `generate_tokens` | 36: `def decorator(generate_tokens):` | |
| `lizard_languages/kotlin.py` | `generate_tokens` | 28: `def generate_tokens(source_code, addition='', token_class=None):` | |
| `lizard_languages/lua.py` | `generate_tokens` | 15: `def generate_tokens(source_code, addition='', token_class=None):` | |
| `lizard_languages/perl.py` | `generate_tokens` | 68: `def generate_tokens(source_code, addition='', token_class=None):` | |
| `lizard_languages/php.py` | `generate_tokens` | 26: `def generate_tokens(source_code, addition='', token_class=None):` | |
| `lizard_languages/plsql.py` | `generate_tokens` | 120: `def generate_tokens(source_code, addition="", token_class=None):` | |
| `lizard_languages/python_tokens.py` | `generate_tokens` | 22: `def generate_tokens(source_code, addition='', token_class=None):` | |
| `lizard_languages/python.py` | `generate_tokens` | 6: `from .python_tokens import generate_tokens as generate_python_tokens` | |
| `lizard_languages/r.py` | `generate_tokens` | 35: `def generate_tokens(source_code, addition='', token_class=None):` | |
| `lizard_languages/ruby.py` | `generate_tokens` | 28: `def generate_tokens(source_code, addition='', token_class=None):` | |
| `lizard_languages/rust.py` | `generate_tokens` | 28: `def generate_tokens(source_code, addition='', token_class=None):` | |
| `lizard_languages/script_language.py` | `generate_tokens` | 31: `return CodeReader.generate_tokens(` | |
| `lizard_languages/st.py` | `generate_tokens` | 61: `def generate_tokens(source_code, addition='', token_class=None):` | |
| `lizard_languages/swift.py` | `generate_tokens` | 104: `def generate_tokens(source_code, addition='', token_class=None):` | |
| `lizard_languages/ttcn.py` | `generate_tokens` | 26: `def generate_tokens(source_code, addition='', token_class=None):` | |
| `lizard_languages/vue.py` | `generate_tokens`, `TypeScriptReader` | 6: `from .typescript import TypeScriptReader` | |

### Test

| File | Terms matched | First matching line | Disposition |
| --- | --- | --- | --- |
| `test/test_languages/testCAndCPP.py` | `generate_tokens` | 440: `generate_tokens = CodeReader.generate_tokens` | |
| `test/test_languages/testES6.py` | `generate_tokens` | 12: `tokens = list(JavaScriptReader.generate_tokens(source))` | |
| `test/test_languages/testFortran.py` | `generate_tokens` | 17: `tokens = list(FortranReader.generate_tokens(source))` | |
| `test/test_languages/testJavaScript.py` | `generate_tokens`, `TypeScriptReader` | 4: `from lizard_languages import TypeScriptReader` | |
| `test/test_languages/testKotlin.py` | `generate_tokens` | 10: `tokens = list(KotlinReader.generate_tokens(source))` | |
| `test/test_languages/testLua.py` | `generate_tokens` | 13: `tokens = list(LuaReader.generate_tokens(source))` | |
| `test/test_languages/testPHP.py` | `generate_tokens` | 12: `tokens = list(PHPReader.generate_tokens(source))` | |
| `test/test_languages/testPythonTokenizer.py` | `generate_tokens` | 11: `tokens = PythonReader.generate_tokens("#'\n''", addition)` | |
| `test/test_languages/testR.py` | `generate_tokens` | 16: `tokens = list(RReader.generate_tokens(source))` | |
| `test/test_languages/testRuby.py` | `generate_tokens` | 15: `tokens = list(RubyReader.generate_tokens(source))` | |
| `test/test_languages/testSwift.py` | `generate_tokens` | 9: `tokens = list(SwiftReader.generate_tokens(source))` | |
| `test/test_languages/testTSX.py` | `generate_tokens`, `TSXReader` | 3: `from lizard_languages import TSXReader` | |
| `test/test_languages/testTypeScript.py` | `generate_tokens`, `TypeScriptReader` | 4: `from lizard_languages import TypeScriptReader` | |
| `test/test_languages/testTypeScriptReaderDispatch.py` | `generate_tokens`, `TypeScriptReader` | 10: `from lizard_languages.typescript import TypeScriptReader` | |
| `test/testCPreprocessorExtension.py` | `generate_tokens` | 6: `generate_tokens = CodeReader.generate_tokens` | |
| `test/testTokenizer.py` | `generate_tokens` | 3: `def generate_tokens(source):` | |

### Test support or stand-in

| File | Terms matched | First matching line | Disposition |
| --- | --- | --- | --- |
| `.agents/skills/lizard-language-support/SKILL.md` | `generate_tokens` | 47: `def generate_tokens(source_code, addition='', token_class=None):` | |

### Journey or feature

none found

### Guidance reader

| File | Terms matched | First matching line | Disposition |
| --- | --- | --- | --- |
| `.planning/slice-plans/007-lexical-js-pr-reviews/PLAN.md` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `testOptionalOperators.py`, `TypeScriptReader` | 37: `` parsers. `TypeScriptReader` in `lizard_languages/typescript.py` (with `` | |
| `.planning/slice-plans/007-lexical-js-pr-reviews/SLICE-PROOF.md` | `test/test_languages/testOptionalOperators.py`, `testOptionalOperators.py`, `generate_tokens` | 21: `` `javascript_templates.py` wired into TypeScript/TSX `generate_tokens`. `` | |
| `docs/adrs/0001-ubiquitous-language-accepted.md` | `generate_tokens` | 40: `` \| **Token** \| Atomic unit produced by a **token generator** (`generate_tokens`). \| `` | |
| `docs/javascript-arrow-boundaries.md` | `TypeScriptReader` | 58: `[testTypeScriptReaderDispatch.py](../test/test_languages/testTypeScriptReaderDispatch.py).` | |
| `docs/reviews/fabio-malpezzi-20261027/pr531/review-findings.md` | `TypeScriptReader`, `TSXReader` | 112: `\| JavaScriptReader → TypeScriptReader/TypeScriptStates \| run: .js API/CLI reproduction and focused JavaScript/ES6 suites \|` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/consumer-supplement.md` | `typescript.py` | 102: `` \| `test/test_languages/testTypeScript.py` \| `get_ts_function_list` \| 7: `def get_ts_function_list(source_code):` \| run: complete reader/extension suites; coord… `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/consumer-trace-1.md` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 40: `` \| `lizard_languages/tsx.py` \| changed path \| `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/consumer-trace-2.md` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testOptionalOperators.py`, `testOptionalOperators.py` | 6: `` \| `docs/reviews/fabio-malpezzi-20261027/pr533/reproduce.py` \| `inputs.json`, `tsx.py`, `JavaScriptFunctionsMixin` \| 18: `from lizard_languages.javascript_funct… `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/consumer-trace-1.md` | `lizard_languages/typescript.py`, `typescript.py` | 53: `` \| `docs/reviews/fabio-malpezzi-20261027/pr532/finish-proof.py` \| `lizard_languages/javascript_functions.py`, `javascript_functions.py` \| 35: `'lizard_languages… `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/consumer-trace-2.md` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testOptionalOperators.py`, `testOptionalOperators.py`, `TypeScriptReader`, `TSXReader` | 7: `` \| `TypeScriptReader` \| supplied \| `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/consumer-trace-4.md` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testOptionalOperators.py`, `testOptionalOperators.py`, `TypeScriptReader` | 8: `` \| `TypeScriptReader` \| supplied \| `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/consumer-trace-2.md` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testOptionalOperators.py`, `testOptionalOperators.py`, `TypeScriptReader` | 9: `` \| `TypeScriptReader` \| supplied \| `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/consumer-trace-dispositions.md` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `generate_tokens`, `TypeScriptReader`, `TSXReader` | 9: `--term LINE_COMMENT --term CCppCommentsMixin --term template_tokens --term generate_tokens` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/consumer-trace-product.md` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `testOptionalOperators.py`, `generate_tokens`, `TypeScriptReader`, `TSXReader` | 9: `` \| `lizard_languages/tsx.py` \| changed path \| `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/consumer-trace.md` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `testOptionalOperators.py`, `generate_tokens`, `TypeScriptReader`, `TSXReader` | 15: `` \| `lizard_languages/tsx.py` \| changed path \| `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr544/README.md` | `tsx.py`, `generate_tokens`, `TypeScriptReader` | 26: `` PFE: reuse `TypeScriptReader` / `CCppCommentsMixin` / `javascript_templates` `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr545/consumer-trace-dispositions.md` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `generate_tokens`, `TypeScriptReader`, `TSXReader` | 10: `--term generate_tokens --term conditions` | |
| `docs/reviews/fabio-malpezzi-20261027/pr545/consumer-trace-product.md` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `generate_tokens`, `TypeScriptReader`, `TSXReader` | 9: `` \| `lizard_languages/typescript.py` \| changed path \| `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr545/consumer-trace.md` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `generate_tokens`, `TypeScriptReader`, `TSXReader` | 41: `` \| `lizard_languages/typescript.py` \| changed path \| `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr545/README.md` | `TypeScriptReader` | 22: `` `TypeScriptReader` / `javascript_templates` — no parallel parser. `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr546/consumer-trace-dispositions.md` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `TypeScriptReader`, `TSXReader` | 9: `--term process_token --term add_condition --term conditions --term TypeScriptReader` | |
| `docs/reviews/fabio-malpezzi-20261027/pr546/consumer-trace-product.md` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `generate_tokens`, `TypeScriptReader`, `TSXReader` | 7: `` \| `lizard_languages/typescript.py` \| changed path \| `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr546/consumer-trace.md` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `generate_tokens`, `TypeScriptReader`, `TSXReader` | 49: `` \| `lizard_languages/typescript.py` \| changed path \| `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr546/README.md` | `tsx.py`, `TypeScriptReader` | 26: `` architectural ownership. PFE: extend existing `TypeScriptReader` condition `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr547/consumer-trace.md` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testOptionalOperators.py`, `testOptionalOperators.py`, `OPTIONAL_NAME`, `generate_tokens`, `TypeScriptReader`, `TSXReader` | 45: `` \| `lizard_languages/tsx.py` \| changed path \| `` | |
| `docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md` | `tsx.py` | 84: `nix develop -c python -m pytest test/test_languages/testJavaScript*.py test/test_languages/testTypeScript*.py test/test_languages/testES6.py test/test_language…` | |
| `ongoing/language-implementation-guide.md` | `typescript.py`, `generate_tokens` | 77: `def generate_tokens(source_code, addition='', token_class=None):` | |
| `prompt-history.md` | `tsx.py`, `typescript.py`, `TypeScriptReader` | 5: `please add support for TSX file similar to @jsx.py and add test similar to @testJSX.py . Consider use @typescript.py and extract common part that support React…` | |
