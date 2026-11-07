# Consumer trace

## Effect terms searched

| Term | Origin |
| --- | --- |
| `docs/reviews/fabio-malpezzi-20261027/pr544/consumer-trace.md` | changed path |
| `consumer-trace.md` | basename of `docs/reviews/fabio-malpezzi-20261027/pr544/consumer-trace.md` |
| `docs/reviews/fabio-malpezzi-20261027/pr544/inline-comments.json` | changed path |
| `inline-comments.json` | basename of `docs/reviews/fabio-malpezzi-20261027/pr544/inline-comments.json` |
| `docs/reviews/fabio-malpezzi-20261027/pr544/metadata.json` | changed path |
| `metadata.json` | basename of `docs/reviews/fabio-malpezzi-20261027/pr544/metadata.json` |
| `lizard_languages/javascript_templates.py` | changed path |
| `javascript_templates.py` | basename of `lizard_languages/javascript_templates.py` |
| `lizard_languages/tsx.py` | changed path |
| `tsx.py` | basename of `lizard_languages/tsx.py` |
| `lizard_languages/typescript.py` | changed path |
| `typescript.py` | basename of `lizard_languages/typescript.py` |
| `test/test_languages/testJavaScript.py` | changed path |
| `testJavaScript.py` | basename of `test/test_languages/testJavaScript.py` |
| `LINE_COMMENT` | supplied |
| `CCppCommentsMixin` | supplied |
| `template_tokens` | supplied |
| `generate_tokens` | supplied |
| `lizard_languages/javascript_templates` | import path of `lizard_languages/javascript_templates.py`; in 21 files, too broad to list consumers by itself; supply it as a `--term` to list them |
| `javascript_templates` | import path of `lizard_languages/javascript_templates.py`; in 21 files, too broad to list consumers by itself; supply it as a `--term` to list them |
| `lizard_languages/tsx` | import path of `lizard_languages/tsx.py`; in 31 files, too broad to list consumers by itself; supply it as a `--term` to list them |
| `tsx` | import path of `lizard_languages/tsx.py`; in 140 files, too broad to list consumers by itself; supply it as a `--term` to list them |
| `lizard_languages/typescript` | import path of `lizard_languages/typescript.py`; in 101 files, too broad to list consumers by itself; supply it as a `--term` to list them |
| `typescript` | import path of `lizard_languages/typescript.py`; in 193 files, too broad to list consumers by itself; supply it as a `--term` to list them |
| `test/test_languages/testJavaScript` | import path of `test/test_languages/testJavaScript.py`; in 40 files, too broad to list consumers by itself; supply it as a `--term` to list them |
| `testJavaScript` | import path of `test/test_languages/testJavaScript.py`; in 53 files, too broad to list consumers by itself; supply it as a `--term` to list them |
| `docs/reviews/fabio-malpezzi-20261027/pr544/consumer-trace` | import path of `docs/reviews/fabio-malpezzi-20261027/pr544/consumer-trace.md` |
| `consumer-trace` | import path of `docs/reviews/fabio-malpezzi-20261027/pr544/consumer-trace.md`; in 22 files, too broad to list consumers by itself; supply it as a `--term` to list them |
| `docs/reviews/fabio-malpezzi-20261027/pr544/inline-comments` | import path of `docs/reviews/fabio-malpezzi-20261027/pr544/inline-comments.json` |
| `inline-comments` | import path of `docs/reviews/fabio-malpezzi-20261027/pr544/inline-comments.json` |
| `docs/reviews/fabio-malpezzi-20261027/pr544/metadata` | import path of `docs/reviews/fabio-malpezzi-20261027/pr544/metadata.json` |
| `metadata` | import path of `docs/reviews/fabio-malpezzi-20261027/pr544/metadata.json`; in 59 files, too broad to list consumers by itself; supply it as a `--term` to list them |
| `/\*.*?\*/\|//[^\r\n\u2028\u2029]*` | removed from `lizard_languages/javascript_templates.py`:12 |
| `Test_JavaScript_line_comment_ending_with_a_backslash` | declared in `test/test_languages/testJavaScript.py` |

## Scope

Whole checkout at `aa5f40638b23`, including uncommitted and untracked files: 1306 tracked and untracked non-ignored files searched with fixed-string, case-insensitive matching.

Changed files not searched: `docs/reviews/fabio-malpezzi-20261027/pr544/consumer-trace.md`, `docs/reviews/fabio-malpezzi-20261027/pr544/inline-comments.json`, `docs/reviews/fabio-malpezzi-20261027/pr544/metadata.json`, `lizard_languages/javascript_templates.py`, `lizard_languages/tsx.py`, `lizard_languages/typescript.py`, `test/test_languages/testJavaScript.py`.

## Located consumers

Kinds are path hints for inspection; each file appears once. Fill every Disposition with `run`, `reused` or `excluded: <reason the effect does not reach its boundary>`.

### Production

| File | Terms matched | First matching line | Disposition |
| --- | --- | --- | --- |
| `docs/reviews/fabio-malpezzi-20261027/controls/manifest.json` | `lizard_languages/javascript_templates.py`, `javascript_templates.py`, `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 73: `"lizard_languages/javascript_templates.py": "1dbff8db76103aa77f09d24209744351829924220ceb48e4a04a1edf57369112",` | |
| `docs/reviews/fabio-malpezzi-20261027/manifest.json` | `lizard_languages/javascript_templates.py`, `javascript_templates.py`, `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 73: `"lizard_languages/javascript_templates.py": "1dbff8db76103aa77f09d24209744351829924220ceb48e4a04a1edf57369112",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr528/528-profile-baseline.json` | `generate_tokens` | 63: `"function": "generate_tokens_with_regex",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr528/528-profile-candidate.json` | `generate_tokens` | 71: `"function": "generate_tokens_with_regex",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr528/manifest.json` | `lizard_languages/javascript_templates.py`, `javascript_templates.py`, `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 73: `"lizard_languages/javascript_templates.py": "1dbff8db76103aa77f09d24209744351829924220ceb48e4a04a1edf57369112",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr528/proposed-patch.json` | `test/test_languages/testJavaScript.py`, `testJavaScript.py` | 12: `"filename": "test/test_languages/testJavaScript.py",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr528/review-input.json` | `test/test_languages/testJavaScript.py`, `testJavaScript.py` | 12: `"body": "Supersedes #504. Since d90c086 (#505) a regex literal is recognized before\nstrings, which fixes most of what #504 addressed: of its 23 tests, 20 pass…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr531/531-focused-candidate-correct-imports.log` | `tsx.py` | 1: `command=['/Users/terryyin/git/lizard/.worktrees/review-correct-and-merge-fabiomalpezzi-s-open-pr/.venv/bin/python', '-c', "import sys,pathlib\nroot=pathlib.Pat…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr531/531-focused-candidate-isolated.log` | `tsx.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py` | 3: `command=['/Users/terryyin/git/lizard/.worktrees/review-correct-and-merge-fabiomalpezzi-s-open-pr/.venv/bin/python', '-c', "import sys,pathlib\nroot=pathlib.Pat…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr531/531-focused-candidate.log` | `tsx.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py` | 1: `command=['/Users/terryyin/git/lizard/.worktrees/review-correct-and-merge-fabiomalpezzi-s-open-pr/.venv/bin/python', '-c', "import sys,pathlib,pytest; root=path…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr531/531-profile-baseline.json` | `lizard_languages/javascript_templates.py`, `javascript_templates.py`, `template_tokens`, `generate_tokens` | 73: `"function": "generate_tokens_with_regex",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr531/531-profile-candidate.json` | `lizard_languages/javascript_templates.py`, `javascript_templates.py`, `template_tokens`, `generate_tokens` | 73: `"function": "generate_tokens_with_regex",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr531/manifest.json` | `lizard_languages/javascript_templates.py`, `javascript_templates.py`, `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 75: `"lizard_languages/javascript_templates.py": "1dbff8db76103aa77f09d24209744351829924220ceb48e4a04a1edf57369112",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/affected-manifest.json` | `lizard_languages/javascript_templates.py`, `javascript_templates.py`, `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 87: `"lizard_languages/javascript_templates.py": "1dbff8db76103aa77f09d24209744351829924220ceb48e4a04a1edf57369112",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/baseline-final-reproduction.log` | `typescript.py` | 6: `{"module": "test.test_languages.testTypeScript", "path": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/baseline/test/test_languages/testTypeScript.py"}` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/baseline-supplemental.log` | `typescript.py` | 6: `{"module": "test.test_languages.testTypeScript", "path": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/baseline/test/test_languages/testTypeScript.py"}` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/consumers-manifest.json` | `lizard_languages/javascript_templates.py`, `javascript_templates.py`, `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 92: `"lizard_languages/javascript_templates.py": "1dbff8db76103aa77f09d24209744351829924220ceb48e4a04a1edf57369112",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/corrected-final-reproduction.log` | `typescript.py` | 6: `{"module": "test.test_languages.testTypeScript", "path": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr532/corrected/test/test_languages/testTypeScript.…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/corrected-supplemental.log` | `typescript.py` | 6: `{"module": "test.test_languages.testTypeScript", "path": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr532/corrected/test/test_languages/testTypeScript.…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/current-base-make.log.part1` | `lizard_languages/javascript_templates.py`, `javascript_templates.py`, `tsx.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py` | 71: `test/test_languages/testJavaScript.py ........................           [ 50%]` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/current-base-make.log.part2` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 9: `lizard_languages/tsx.py                           109      5    95%   94, 125, 149, 168-169` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/current-base-make.log.part3` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `CCppCommentsMixin` | 12: `lizard_languages/tsx.py:37:12: R1737: [1;3;35mUse 'yield from' directly instead of yielding each element one by one[0m ([1;3;35muse-yield-from[0m)` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/current-base-proof.json` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 9: `"lizard_languages/typescript.py",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/current-base-pytest.log` | `tsx.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py` | 70: `test/test_languages/testJavaScript.py ........................           [ 50%]` | |
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
| `docs/reviews/fabio-malpezzi-20261027/pr533/manifest.json` | `lizard_languages/javascript_templates.py`, `javascript_templates.py`, `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 1: `{"revision": "2e21e404c1ab9f4e0b49acc80816e816629d0670", "target": "7ac449f59b3f155fd159ce9aad6794a422045632", "head": "15423852c04aed302f684395fe9522251d88a2f…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr533/raw-profile.json` | `generate_tokens` | 53: `"function": "generate_tokens_with_regex",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr533/raw-profile.log` | `generate_tokens` | 1: `{"checkout": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/raw", "loops": 5000, "observed_metrics": [["f", 1, 4, 2, 4, 12], ["after", 5, 5, 1, 1, 7]…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr533/reproduce.py` | `tsx.py` | 35: `'testTSX.py', 'testVue.py')` | |
| `docs/reviews/fabio-malpezzi-20261027/pr533/target-profile.json` | `generate_tokens` | 45: `"function": "generate_tokens_with_regex",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr533/target-profile.log` | `generate_tokens` | 1: `{"checkout": "/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr533/target", "loops": 5000, "observed_metrics": [["f", 1, 4, 2, 4, 12], ["after", 5, 5, 1, 1,…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/affected-manifest.json` | `lizard_languages/javascript_templates.py`, `javascript_templates.py`, `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 100: `"lizard_languages/javascript_templates.py": "1dbff8db76103aa77f09d24209744351829924220ceb48e4a04a1edf57369112",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/consumers-manifest.json` | `lizard_languages/javascript_templates.py`, `javascript_templates.py`, `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 105: `"lizard_languages/javascript_templates.py": "1dbff8db76103aa77f09d24209744351829924220ceb48e4a04a1edf57369112",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/current-base-make.log.part1` | `lizard_languages/javascript_templates.py`, `javascript_templates.py`, `tsx.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py` | 71: `test/test_languages/testJavaScript.py ........................           [ 49%]` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/current-base-make.log.part2` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 11: `lizard_languages/tsx.py                           109      5    95%   94, 125, 149, 168-169` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/current-base-make.log.part3` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `CCppCommentsMixin` | 14: `lizard_languages/tsx.py:37:12: R1737: [1;3;35mUse 'yield from' directly instead of yielding each element one by one[0m ([1;3;35muse-yield-from[0m)` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/current-base-pytest.log` | `tsx.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py` | 70: `test/test_languages/testJavaScript.py ........................           [ 49%]` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/focused-command.json` | `tsx.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py` | 7: `"test/test_languages/testJavaScript.py",` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/focused.py` | `tsx.py` | 27: `('testJavaScript*.py', 'testTypeScript*.py', 'testES6.py', 'testTSX.py',` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/snapshot-hashes.json` | `lizard_languages/javascript_templates.py`, `javascript_templates.py`, `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 1: `[{"path":"lizard.py","target":"fc83d74b7d74e6b579d81808451e89661c26d599624c5acc4a41614a46253c71","corrected":"fc83d74b7d74e6b579d81808451e89661c26d599624c5acc4…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/adapted-profile.log` | `lizard_languages/javascript_templates.py`, `javascript_templates.py`, `lizard_languages/typescript.py`, `typescript.py`, `template_tokens`, `generate_tokens` | 19: `123500    0.033    0.000    0.156    0.000 /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr536/adapted/lizard_languages/js_style_regex_expression.py:16(gen…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/bootstrap.py` | `inline-comments.json`, `metadata.json`, `inline-comments` | 81: `('metadata.json', ['gh', 'pr', 'view', '536', '--repo', 'terryyin/lizard',` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/consumer-trace-1.md.part1` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py` | 63: `` \| `docs/reviews/fabio-malpezzi-20261027/pr532/current-base-make.log.part3` \| `lizard_languages/javascript_bindings.py`, `javascript_bindings.py`, `lizard_langu… `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/consumer-trace-1.md.part2` | `typescript.py` | 12: `` \| `test/test_languages/testTypeScript.py` \| `parameter_count` \| 16: `[(f.name, f.cyclomatic_complexity, f.parameter_count)` \|run: snapshot reader groups, suppl… `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/manifest.json` | `lizard_languages/javascript_templates.py`, `javascript_templates.py`, `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 1: `{"affected/annotations.ts": "93f5227836b388139b742a6ded0234207d1f3020f7bd302a4e107cbf99155386", "affected/arrow-abandoned.js": "56a151a284b6035d556f04483910b8f…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/parent-profile.log` | `lizard_languages/javascript_templates.py`, `javascript_templates.py`, `lizard_languages/typescript.py`, `typescript.py`, `template_tokens`, `generate_tokens` | 19: `123500    0.033    0.000    0.151    0.000 /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr536/parent/lizard_languages/js_style_regex_expression.py:16(gene…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/raw-profile.log` | `lizard_languages/javascript_templates.py`, `javascript_templates.py`, `lizard_languages/typescript.py`, `typescript.py`, `template_tokens`, `generate_tokens` | 19: `123500    0.033    0.000    0.156    0.000 /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr536/raw/lizard_languages/js_style_regex_expression.py:16(generat…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/run-proof.py` | `tsx.py` | 23: `for name in ('testES6.py', 'testTSX.py', 'testOptionalOperators.py')]` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/source-hashes.json` | `lizard_languages/javascript_templates.py`, `javascript_templates.py`, `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 1: `{"target": {"lizard_core/__init__.py": "345df9bf6abd477b82b7546d212473cf78aa1a15450be0f75ba32603206d066c", "lizard_core/analysis.py": "4e6c15d0b00aff70b5264d40…` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/target-profile.log` | `lizard_languages/javascript_templates.py`, `javascript_templates.py`, `lizard_languages/typescript.py`, `typescript.py`, `template_tokens`, `generate_tokens` | 19: `123500    0.032    0.000    0.148    0.000 /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr536/target/lizard_languages/js_style_regex_expression.py:16(gene…` | |
| `lizard_core/file_analyzer.py` | `generate_tokens` | 97: `tokens = reader.generate_tokens(code)` | |
| `lizard_languages/clike.py` | `CCppCommentsMixin`, `generate_tokens` | 12: `# See #451, #478; subgroup-free per CodeReader.generate_tokens.` | |
| `lizard_languages/code_reader.py` | `generate_tokens` | 136: `def generate_tokens(source_code, addition='', token_class=None):` | |
| `lizard_languages/csharp.py` | `generate_tokens` | 28: `def generate_tokens(source_code, addition='', token_class=None):` | |
| `lizard_languages/erlang.py` | `generate_tokens` | 36: `def generate_tokens(source_code, addition='', token_class=None):` | |
| `lizard_languages/fortran.py` | `generate_tokens` | 36: `def generate_tokens(source_code, addition='', token_class=None):` | |
| `lizard_languages/go.py` | `CCppCommentsMixin`, `generate_tokens` | 6: `from .clike import CCppCommentsMixin` | |
| `lizard_languages/js_style_regex_expression.py` | `generate_tokens` | 36: `def decorator(generate_tokens):` | |
| `lizard_languages/kotlin.py` | `CCppCommentsMixin`, `generate_tokens` | 5: `from .clike import CCppCommentsMixin` | |
| `lizard_languages/lua.py` | `generate_tokens` | 15: `def generate_tokens(source_code, addition='', token_class=None):` | |
| `lizard_languages/perl.py` | `generate_tokens` | 68: `def generate_tokens(source_code, addition='', token_class=None):` | |
| `lizard_languages/php.py` | `CCppCommentsMixin`, `generate_tokens` | 7: `from .clike import CCppCommentsMixin` | |
| `lizard_languages/plsql.py` | `CCppCommentsMixin`, `generate_tokens` | 36: `from .clike import CCppCommentsMixin` | |
| `lizard_languages/python_tokens.py` | `generate_tokens` | 22: `def generate_tokens(source_code, addition='', token_class=None):` | |
| `lizard_languages/python.py` | `generate_tokens` | 6: `from .python_tokens import generate_tokens as generate_python_tokens` | |
| `lizard_languages/r.py` | `generate_tokens` | 35: `def generate_tokens(source_code, addition='', token_class=None):` | |
| `lizard_languages/ruby.py` | `generate_tokens` | 28: `def generate_tokens(source_code, addition='', token_class=None):` | |
| `lizard_languages/rust.py` | `CCppCommentsMixin`, `generate_tokens` | 6: `from .clike import CCppCommentsMixin` | |
| `lizard_languages/scala.py` | `CCppCommentsMixin` | 5: `from .clike import CCppCommentsMixin` | |
| `lizard_languages/script_language.py` | `generate_tokens` | 31: `return CodeReader.generate_tokens(` | |
| `lizard_languages/solidity.py` | `CCppCommentsMixin` | 5: `from .clike import CCppCommentsMixin` | |
| `lizard_languages/st.py` | `generate_tokens` | 61: `def generate_tokens(source_code, addition='', token_class=None):` | |
| `lizard_languages/swift.py` | `CCppCommentsMixin`, `generate_tokens` | 6: `from .clike import CCppCommentsMixin` | |
| `lizard_languages/tnsdl.py` | `CCppCommentsMixin` | 7: `from .clike import CCppCommentsMixin` | |
| `lizard_languages/ttcn.py` | `generate_tokens` | 26: `def generate_tokens(source_code, addition='', token_class=None):` | |
| `lizard_languages/vue.py` | `generate_tokens` | 19: `def generate_tokens(source_code, addition='', token_class=None):` | |
| `lizard_languages/zig.py` | `CCppCommentsMixin` | 7: `from .clike import CCppCommentsMixin` | |

### Test

| File | Terms matched | First matching line | Disposition |
| --- | --- | --- | --- |
| `test/test_languages/testCAndCPP.py` | `generate_tokens` | 440: `generate_tokens = CodeReader.generate_tokens` | |
| `test/test_languages/testES6.py` | `generate_tokens` | 12: `tokens = list(JavaScriptReader.generate_tokens(source))` | |
| `test/test_languages/testFortran.py` | `generate_tokens` | 17: `tokens = list(FortranReader.generate_tokens(source))` | |
| `test/test_languages/testKotlin.py` | `generate_tokens` | 10: `tokens = list(KotlinReader.generate_tokens(source))` | |
| `test/test_languages/testLua.py` | `generate_tokens` | 13: `tokens = list(LuaReader.generate_tokens(source))` | |
| `test/test_languages/testPerl.py` | `LINE_COMMENT` | 619: `def test_inline_comment_after_code(self):` | |
| `test/test_languages/testPHP.py` | `generate_tokens` | 12: `tokens = list(PHPReader.generate_tokens(source))` | |
| `test/test_languages/testPLSQL.py` | `LINE_COMMENT` | 156: `def test_single_line_comment(self):` | |
| `test/test_languages/testPythonTokenizer.py` | `generate_tokens` | 11: `tokens = PythonReader.generate_tokens("#'\n''", addition)` | |
| `test/test_languages/testR.py` | `generate_tokens` | 16: `tokens = list(RReader.generate_tokens(source))` | |
| `test/test_languages/testRuby.py` | `generate_tokens` | 15: `tokens = list(RubyReader.generate_tokens(source))` | |
| `test/test_languages/testSwift.py` | `generate_tokens` | 9: `tokens = list(SwiftReader.generate_tokens(source))` | |
| `test/test_languages/testTSX.py` | `generate_tokens` | 12: `tokens = list(TSXReader.generate_tokens(source))` | |
| `test/test_languages/testTypeScript.py` | `LINE_COMMENT`, `generate_tokens` | 95: `def test_template_line_comment_and_multiline_text_preserve_ranges(self):` | |
| `test/test_languages/testTypeScriptReaderDispatch.py` | `generate_tokens` | 62: `tokens = reader.generate_tokens(SOURCE)` | |
| `test/testBasicFunctionInfo.py` | `LINE_COMMENT` | 89: `def test_having_multiple_line_comments(self):` | |
| `test/testCPreprocessorExtension.py` | `generate_tokens` | 6: `generate_tokens = CodeReader.generate_tokens` | |
| `test/testTokenizer.py` | `generate_tokens` | 3: `def generate_tokens(source):` | |

### Test support or stand-in

| File | Terms matched | First matching line | Disposition |
| --- | --- | --- | --- |
| `.agents/skills/lizard-language-support/SKILL.md` | `CCppCommentsMixin`, `generate_tokens` | 27: `from .clike import CCppCommentsMixin  # If language has C-style comments` | |

### Journey or feature

none found

### Guidance reader

| File | Terms matched | First matching line | Disposition |
| --- | --- | --- | --- |
| `.planning/slice-plans/007-lexical-js-pr-reviews/PLAN.md` | `lizard_languages/javascript_templates.py`, `javascript_templates.py`, `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `CCppCommentsMixin` | 37: `` parsers. `TypeScriptReader` in `lizard_languages/typescript.py` (with `` | |
| `docs/adrs/0001-ubiquitous-language-accepted.md` | `generate_tokens` | 40: `` \| **Token** \| Atomic unit produced by a **token generator** (`generate_tokens`). \| `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/consumer-supplement.md` | `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py` | 94: `` \| `test/test_languages/testJavaScript.py` \| `get_js_function_list` \| 7: `def get_js_function_list(source_code):` \| run: complete reader/extension suites; coord… `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/consumer-trace-1.md` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py` | 40: `` \| `lizard_languages/tsx.py` \| changed path \| `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr532/consumer-trace-2.md` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py` | 6: `` \| `docs/reviews/fabio-malpezzi-20261027/pr533/reproduce.py` \| `inputs.json`, `tsx.py`, `JavaScriptFunctionsMixin` \| 18: `from lizard_languages.javascript_funct… `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/consumer-trace-1.md` | `lizard_languages/typescript.py`, `typescript.py` | 53: `` \| `docs/reviews/fabio-malpezzi-20261027/pr532/finish-proof.py` \| `lizard_languages/javascript_functions.py`, `javascript_functions.py` \| 35: `'lizard_languages… `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/consumer-trace-2.md` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py`, `CCppCommentsMixin` | 92: `` \| `lizard_languages/tsx.py` \| `TypeScriptReader`, `TSXReader` \| 10: `JS_IDENTIFIER, NULLISH_COALESCING, OPTIONAL_ACCESS, TypeScriptReader)` \|run: focused628/5,… `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr535/consumer-trace-4.md` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py`, `CCppCommentsMixin` | 40: `` \| `lizard_languages/tsx.py` \| `TypeScriptReader` \| 10: `JS_IDENTIFIER, NULLISH_COALESCING, OPTIONAL_ACCESS, TypeScriptReader)` \|run: focused628/5, supplemental… `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/consumer-trace-2.md` | `lizard_languages/tsx.py`, `tsx.py`, `lizard_languages/typescript.py`, `typescript.py`, `test/test_languages/testJavaScript.py`, `testJavaScript.py`, `CCppCommentsMixin` | 38: `` \| `lizard_languages/tsx.py` \| `TypeScriptReader` \| 10: `JS_IDENTIFIER, NULLISH_COALESCING, OPTIONAL_ACCESS, TypeScriptReader)` \|run: snapshot reader groups, su… `` | |
| `docs/reviews/fabio-malpezzi-20261027/pr536/README.md` | `inline-comments.json`, `metadata.json`, `inline-comments` | 31: `metadata.json, inline-comments.json, review-threads.json and` | |
| `docs/reviews/fabio-malpezzi-20261027/REVIEW-CONTRACT.md` | `tsx.py`, `CCppCommentsMixin` | 84: `nix develop -c python -m pytest test/test_languages/testJavaScript*.py test/test_languages/testTypeScript*.py test/test_languages/testES6.py test/test_language…` | |
| `ongoing/language-implementation-guide.md` | `typescript.py`, `CCppCommentsMixin`, `generate_tokens` | 21: `from .clike import CCppCommentsMixin  # If language has C-style comments` | |
| `prompt-history.md` | `tsx.py`, `typescript.py`, `testJavaScript.py` | 1: `This project (Lizard) support JSX files, but the implementation is together with javascript support @javascript.py . Please extract the jsx support from the ja…` | |
