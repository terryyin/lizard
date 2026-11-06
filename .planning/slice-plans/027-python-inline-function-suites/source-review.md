# Source-only review — SEED-042#python-inline-function-suites

## Launch and input

Fresh general-purpose reviewer with no conversation inheritance. Inputs were
only the selected story Goal, Scope, key examples, exclusions, and deferred
promises from `.planning/seeds/SEED-042-python-inline-function-suites.md`
(`### Judge Python inline function suites`), plus explicit note that human
amendments were none. Instruction: use only these inputs; read no other
project files; change nothing; return numbered commitments.

Returned 2026-11-06 during slice 1 acceptance.

## Commitments

| Id | Source commitment | Slice | Observation or obligation |
| --- | --- | --- | --- |
| 1 | Python users and maintainers get a recorded decision about functions whose suite is on the `def` line. | slice 1 | observed: PR comment https://github.com/terryyin/lizard/pull/530#issuecomment-6687330144 records reject disposition; plan evidence/summary.json |
| 2 | The Python reader changes only when both of the following are shown: (a) those functions are missing, and (b) the slowdown stays within 3%. | slice 1 | observed: reader unchanged on trunk (git clean @ d189528); slowdown not within 3% (evidence/candidate*-api*.json) |
| 3 | Until both conditions in 2 are shown, trunk stays as it is and PR #530 stays unmerged. | slice 1 | observed: git status clean at d189528; gh pr view 530 state OPEN; no reader land |
| 4 | Beneficiaries are people who analyze Python that uses one-line function suites, and maintainers who would carry a slower Python reader. | slice 1 | observed: beneficiary context retained; no delivery change required |
| 5 | Source is PR #530, head `2948b38ed0bf70c5a4c39163b45dfa50c3020314` at review on 2026-11-06. | slice 1 | observed: gh pr view 530 headRefOid 2948b38ed0bf70c5a4c39163b45dfa50c3020314 |
| 6 | Process execution SEED-040#process-pr-530 is dropped. | slice 1 | observed: story states SEED-040 dropped; not revived in product tree |
| 7 | The uncommitted reader experiment from that dropped process is not product code and is not a commitment to accept the pull request. | slice 1 | observed: owned checkout has no SEED-040 experiment; only evaluation under /tmp then restored |
| 8 | That head alone still reports `def value(): """A value."""; return 1` with NLOC 0; a disposition that lands must correct that as well. | slice 1 | observed: reject path — no land; NLOC-0 head noted in https://github.com/terryyin/lizard/pull/530#issuecomment-6687330144; land would have required NLOC 1 |
| 9 | Decide whether inline Python suites belong in Lizard. | slice 1 | observed: decision recorded: do not land inline suites under current <3% gate (https://github.com/terryyin/lizard/pull/530#issuecomment-6687330144) |
| 10 | Change the Python reader only after both required conditions (missing-function proof and slowdown bound) hold. | slice 1 | observed: no python.py/python_structure.py change on owned branch; conditions unmet |
| 11 | PR #530 is a candidate and a source of examples, not an acceptance. | slice 1 | observed: PR treated as candidate only; left OPEN unmerged |
| 12 | Function discovery and suite boundaries stay in the existing Python reader (`python.py` / `python_structure.py`). | slice 1 | observed: evaluation stayed in python.py/python_structure.py overlays only; no second parser |
| 13 | Do not add a second parser. | slice 1 | observed: no second parser added; tree clean of new reader modules |
| 14 | On current trunk, confirm the key examples below before any reader change. | slice 1 | observed: lizard.analyze_file on trunk: STREAM [('kept',2,4)]; NESTED [('outer.inner',1,1)]; DOC [] |
| 15 | Qualification (historical observation, not a new promise): the 2026-11-06 check still found no functions for the `Stream` methods. | slice 1 | observed: reconfirmed STREAM methods absent on trunk (only kept) |
| 16 | A reader change lands only when its paired 95% upper slowdown bound is strictly below 3% on each of the following, each compared with current trunk. | slice 1 | observed: no land; all required workloads checked against limit_pct 3.0 |
| 16.1 | the affected API | slice 1 | observed: candidate1-api.json CI +11.83..+13.60; candidate2-alt-api.json +1.92..+3.31; both bound-not-met |
| 16.2 | public parameter names and counts | slice 1 | observed: candidate1-api-parameters.json +14.29..+15.55; candidate2-alt-api-parameters.json +1.94..+3.29; both bound-not-met |
| 16.3 | broader CLI analysis | slice 1 | observed: candidate1-cli.json +9.71..+12.24 bound-not-met; candidate2-alt-cli.json +1.05..+2.37 passes alone |
| 17 | A result at 3% or above does not land. | slice 1 | observed: neither candidate landed; verdicts bound-not-met where upper >= 3 |
| 18 | Qualification (historical observation): the 2026-11-06 attempt did not meet that bound — six alternating passes of a correct candidate were about 9.7% slower on 281 Python files; a paired API interval was +2.2% to +13.6%; one bracket-depth lookup per token alone was about +3.1%. | slice 1 | observed: historical note retained in story/plan; new measurements also fail the bound |
| 19 | The missing functions have to be real on the examples below. | slice 1 | observed: trunk examples show Stream read/close and nested outer missing before any change |
| 20 | A performance pass does not by itself accept a reader change. | slice 1 | observed: reject despite behavior-green candidates — performance gate blocked land |
| 21 | A behavior pass does not by itself accept a slower reader. | slice 1 | observed: behavior-pass candidates left unlanded because upper bound not <3% |
| 22 | Land a reader change only when both conditions hold (missing functions shown and slowdown strictly below 3%). | slice 1 | observed: both conditions not jointly held; no reader land |
| 23 | If the pull request is still the vehicle for an accepted change, preserve contributor history. | slice 1 | observed: no accepted change — contributor-history vehicle unused; PR left as-is |
| 24 | Otherwise record the judgment on the pull request and leave it unmerged. | slice 1 | observed: judgment posted https://github.com/terryyin/lizard/pull/530#issuecomment-6687330144; PR remains OPEN unmerged |
| 25 | If a change is accepted, a shared-line docstring still counts the `def` line: `def value(): """A value."""; return 1` reports `value` with NLOC 1. | slice 1 | observed: accept-path unmet — no land; shared-line NLOC 1 not shipped |
| 26 | If a change is accepted, a block function keeps the metrics it already has. | slice 1 | observed: accept-path unmet — no land; block kept metrics already on trunk CCN2 NLOC4 |
| 27 | The multiline f-string `end_line` limit stays out of this story. | slice 1 | observed: no multiline f-string end_line work in owned changes |
| 28 | Deferred: the multiline f-string `end_line` limit. | slice 1 | observed: deferred f-string end_line untouched |
| 29 | Deferred: closed PRs #526 and #529. | slice 1 | observed: closed PRs 526/529 not reopened or merged |
| 30 | Deferred: other languages. | slice 1 | observed: no other-language reader changes |
| 31 | Do not merge PR #530 to close a review. | slice 1 | observed: PR 530 not merged (state OPEN) |
| 32 | Do not treat the dropped experiment as accepted code. | slice 1 | observed: dropped experiment not committed; overlays restored |
| 33 | Do not add a second Python parser. | slice 1 | observed: no second parser added |
| 34 | Given `class Stream` with `def read(self) -> str: ...` and `def close(self) -> None: ...` on current trunk, analysis lists no functions. | slice 1 | observed: trunk analyze STREAM lists no read/close — only kept |
| 35 | Given a block function such as `def kept(x):` with an `if`, analysis still reports `kept` with CCN 2. | slice 1 | observed: trunk analyze kept CCN 2 present |
| 36 | Given only `def outer():` / `def inner(): return 1`, analysis lists `outer.inner` and drops `outer`. | slice 1 | observed: trunk analyze NESTED only outer.inner |
| 37 | Given those `Stream` methods, when both the missing functions and a slowdown within 3% are shown: | slice 1 | observed: journey trigger unmet — slowdown not within 3%; reject path taken |
| 37.1 | analysis reports `read` and `close` | slice 1 | observed: accept journey not triggered; read/close not shipped to trunk |
| 37.2 | a nested inline keeps both the outer and inner functions | slice 1 | observed: accept journey not triggered; nested outer keep not shipped to trunk |
| 37.3 | `def value(): """A value."""; return 1` reports `value` with NLOC 1 | slice 1 | observed: accept journey not triggered; docstring NLOC 1 not shipped to trunk |
| 38 | Given a performance result whose upper bound is not strictly below 3%, the reader on trunk stays unchanged and PR #530 stays open. | slice 1 | observed: owned tree clean (reader unchanged); PR 530 OPEN after failed <3% upper bounds |
