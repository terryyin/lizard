# PR #532 review return

Disposition: merged as9bc44563aad42b780ff0264aa5a3a972a583b423 at corrected head
2a8af8acea20c53a0c57c75ff5d179d2db8a2eff. All fourteen final CI jobs passed.
Thank-you: https://github.com/terryyin/lizard/pull/532#issuecomment-6436143243.

Source: https://github.com/terryyin/lizard/pull/532. Frozen/refreshed head
`d0713bf5b4dcf4bf24453156a3756688627abbc0`, base
`01afe9c228ba2a3dc5253259d8415d2ef47f0b71`; initially open and unmerged.
No prior discussion/reviews. Historical four-job CI was superseded by final fourteen-job proof.

Target `33807306953535ebda5c0fa42797b1e5e7ef73d1`; refreshed trunk
`d8e8e9af75b6d05da005e981af5f8867cced5d55` has unchanged analyzer sources but
a new website consumer. Detailed evidence, full patch, metadata, commands,
observations and raw timings are in
[review-findings.md](../../../docs/reviews/fabio-malpezzi-20261027/pr532/review-findings.md)
and [README.md](../../../docs/reviews/fabio-malpezzi-20261027/pr532/README.md).

## Behavior and bounded correction

The report is real: target render=1/round=1, required 3/2. Raw PR fixes these but
loses quoted/numeric/computed destructured parameters (2→1), and public counts
still omit $, $$ and a$ while normalized names lose sigils. The narrow corrected
candidate passes all 21 retained source inputs and 34 new analyzer/consumer tests.

Before product edits, correction plus cleanup was estimated 12–15 minutes,
excluding review and proof. Five small production files reuse existing owners:
JSFunctions keeps the contributor's identifier/annotation idea; Bindings exposes
root depth so destructuring colons remain bindings; TS/TSX share a complete
dollar identifier token; FunctionInfo's one matcher accepts dollar names.
The public facade, existing core ownership and Accepted ADR-0000/0001 hold.
There is no parser-specific core switch or parallel parameter count.

The new small test module owns author examples and nearby controls. Original
large JS/TS author test files remain byte-identical in the execution checkout.
No unrelated arrow/newline or this-declaration change was made.

## Current proof

- Red: target 10 failed / 23 passed; raw 20 failed / 13 passed for the new original 33 tests.
- All language/extension suites plus analyzer/output/CLI/options:1465 passed / 5 skipped.
- Additional output/threshold/duplicate consumers:51 passed, including all 34 new tests.
- Coordinator full suite on the exact correction:1721 passed / 7 skipped, 24 warnings.
- Coordinator make exit0: coverage tests, pycodestyle, pylint --exit-zero9.12.
- Installed consumer trace and supplemental helper/historical replay trace name
  every located consumer and its run/reused/excluded disposition.

Parameter correctness is observed through analyze_file and actual output
consumers, including the new HTTP consumer below. Snapshot reproductions assert facade, registry, function owner,
TS state, FunctionInfo and test-helper paths. The exact source setup and named
assertions are retained in the detailed findings; no parser mocks are used.

PHP($value,$tail) remains count 1 rather than 2 on baseline/target/raw/corrected,
with unchanged full_parameters=['$value $tail']; corrected normalized last name
retains '$'. A single-parameter control does not claim that comma bug fixed.
An unrelated generic function declaration is absent on target/raw and retained
as pre-existing. Generic annotations are separately covered.
PR #535's original child counts this declaration correctly (1 versus 2 on #532);
its child diff uses532's type state. This is a later separate promise, not a
prerequisite for532 or a new backlog supplier agreement.

## Performance and remaining gates

Default incremental/cumulative equivalence is established by matching actual
API/broad CLI outputs and unused-default-NS-only source differences. The core
change invalidates reuse of earlier ENS timing; the separate G6 comparison passes.

| Corrected workload | Paired 95% slowdown interval | Result |
| --- | --- | --- |
| Related API | [-1.0915%,+0.0035%] | passes |
| Public parameter names/counts | [+1.1198%,+2.8417%] | passes |
| Broad CLI,1.2-second calibration | [-3.7154%,+10.0648%] | inconclusive |
| Broad CLI,8-second calibration | [-4.6621%,+5.6704%] | inconclusive |
| Broad CLI,quieter fixed 40-pair run | [-1.1537%,+2.3613%] | passes |
| Cumulative broad CLI -ENS,8-second calibration | [-1.4399%,+0.4624%] | passes |

Raw API timing passes but raw correctness does not; raw broad CLI is also
inconclusive. All 20 pairs per workload are preserved. One bounded profile checks
identical complete CLI output and source paths; it cannot establish the 3% gate.
Original profile setup failed from legacy Python 2 profile.py shadowing stdlib;
the exact terminal stderr and corrected /private/tmp setup are retained.

After the implementation return, root selected one fixed
40-pair, eight-second-calibrated ordinary CLI run: fresh load fell to 10.99
on 16 CPUs and the later ENS log-ratio SD fell from 10.99% to 2.04%.
The prospective decision, unchanged snapshot/hash assertions and every duration
are owned by final-cli.py and final-cli-decision.json. The terminal result passes:
41 CLI invocations per batch, medians 7.31951/7.29343 s, SD 0.31302/0.42197 s,
median change -0.3562%, paired geometric +0.58845%, upper bound +2.36131%.
All 40 pairs are retained; warmed batches remained above the source's one-second minimum.
The cumulative ENS
terminal verdict is retained. The first HTTP attempt lacked a runtime and is
retained as observed:false. Root then prepared the complete anticipated merge
tree on d8e8e9a, including both authored tests, in an isolated validation worktree.
Its own Nix setup installed current-trunk dependencies: full pytest1751/7,
make exit0, and real HTTP render=3/round=2/dollars=3/destructuring=2 all pass.
current-base-proof.json retains source-path assertions and tree c7de325e738d18d493fe401a686fc53e3f7d4e51.
Trunk subsequently advanced to40d89f7 with only one agent-profile JSON added.
Runtime/test/workflow/setup bytes match; current-base-refresh.json records valid
proof reuse and updated anticipated merge tree eab2c48635db788e72ba142c6935d483e4a639ef.
Fresh independent refactor_532 returned "none — already clean" and the exact
REFACTOR COMPLETE marker, with no edits or unnecessary verification. It confirmed
all product/test snapshot bytes, consumer dispositions and unchanged accepted proof.
Agent-commit then created contributor correction 2a8af8acea20c53a0c57c75ff5d179d2db8a2eff;
the non-forced fork push succeeded. GitHub merge ref c2c46ce6 has parents40d89f7
and2a8af8a, with exact validated tree eab2c486. Final CI run40466422315 completed
successfully: all14 Ubuntu/Windows jobs across Python3.8–3.14. A final head/base
refresh preceded the exact-head guarded merge; the actual landed tree and parents
match the reviewed ones. Thank-you comment6436143243 succeeded after merge.
The clean contributor worktree was retired; the validation worktree fast-forwarded
to the landed merge with no remaining changes and is retained for closure.
The fresh-delivery whitespace gate refused four new evidence copies with blank
lines at EOF before any commit. Root trimmed only those copies and aligned both
retention helpers' EOF normalization; original logs and accepted runtime proof
are unchanged. The same fresh operation owns the mechanical retry.
That retry delivered c3ca2c73d3c74b5caffe9b54929d0c14945cdb77, base33807306,
with the observer reused and no reconciliation. The existing thank-you was
updated with an immutable link to the published durable review evidence.
No slice status, commit, push, merge or comment was made by this implementation.

## Independent source comparison

Fresh source-only reviewer source_review_6 derived the seed's commitments in
this conversation without reading the plan, implementation or proof. Root
compared that return with the literal fixtures, analyzer/consumer assertions,
whole-concept trace, estimate and unfiltered performance records above. The
reported examples and valid adjacent bindings pass; raw regressions and the
pre-existing PHP/generic limits remain explicit. The fifteen-minute exception
does not waive measurement or final gates. All performance, current-base HTTP,
independent refactoring, final CI/head/base, guarded merge and thanks are observed.
The frozen set and other PRs remain owned by their
ordered slices; no source promise is silently removed.
