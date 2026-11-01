# Independent call-group refactor

Identity: SEED-023#correct-pr525-binding-boundaries. Checkout and branch:
`/Users/terryyin/git/lizard/.worktrees/correct-pr525-binding-boundaries`,
`codex/correct-pr525-binding-boundaries`, HEAD `4a2081598ac8`.

Outcome: **none — already clean**. No product/test edits, renames, extractions,
splits or deletions. No commits, pushes, plan changes or observer operations.

The ordered read-only checks covered duplication, naming, shotgun surgery,
dead/redundant code, behavioral tests and file size. The new
`JavaScriptFunctionsMixin._read_parenthesis_group` centralizes existing
`parameter_group` routing; actual/provisional formals retain binding frames,
default complexity and summary/parameter ownership. Proven calls reuse normal
nested syntax readers. `_call_group` expresses enclosing argument-comma
ownership without another bracket counter or token wrapper. Object/statement
call dispatch and positive named-formal eligibility remain cohesive.

Untouched representations inspected: binding summaries/initializer readers,
optimistic named-arrow paths, object members/computed names, signature lookahead,
CodeStateMachine nesting/replay and reader/custom-root protocols. Their separate
responsibilities justify the dispatch differences. No disputed restriction,
cross-subsystem candidate or ADR-0001 naming/metric conflict emerged.

Current diff file sizes: plan205, JavaScriptFunctions208, JavaScriptObjects126,
TypeScriptStates245; implicated binding owner204. All are under250 lines.
Generated raw trace/hash evidence remains intact.

## Proof correspondence

All106 production hashes still match the accepted
`cost-return/post-edit-arguments-profile.json`. No accepted implementation,
setup, assertion or consuming boundary was invalidated. Reuse:

- cost-return/affected-final.txt:897 passed,5existing skips. Real analyzer sources
  in testJavaScriptBindingBoundaries.py assert authored/callback/successor
  metrics, call groups at113, typed/ternary formals at90, defaults at101,
  generic continuation at122 and NS/nonstrict metrics at131.
- cost-return/consumers-final.txt:499 passed,2existing skips. Actual CLI CSV
  assertions at141 and Flask JSON at155 observe name/CCN/count/span results;
  testJavaScriptFormalSlots.py and testJavaScriptArrowParameters.py assert
  formal names/slots/counts and actual output/threshold values.
- cost-return/located-consumers-final.txt:42 passed. Actual output/forgiveness
  and application smoke observations.
- Original red observations, eleven authored expectations, G1–G6 dispositions,
  immutable inputs and unfavorable attempts remain unchanged.

Tests: **skipped — no refactor edits**, as required by the refactor skill.
`git diff --check` exited0. `correspondence.json` retains complete hash equality.

## Consumer trace and remaining proof

Fresh installed consumer-trace command and terminal result are in
trace-command.json; raw trace and per-path dispositions are adjacent.301paths
are all accounted for, including six newly retained documentary evidence paths.
The measurement worker is treated as an actual public-parameter consumer:
semantic/profile observations are reused, while qualified timing stays pending.
No runtime consumer is excused from final qualification through an evidence
path classification. The unchanged frontend renderer's input contract is
observed by HTTP tests; rendered browser behavior was not rerun.

Exclusive foreground Nix ownership was retained and every invocation terminated.
No heavy test/measurement was launched. Territory.local remains the retained
serial host; unknown unrelated work was left alone. Root must reconsider host
conditions before final performance, then run strict paired API/argument/NS/CLI
comparisons, applicable parent comparisons and full pytest/make on these bytes.
G7 remains open with that coordinator; no timing or merge clearance is claimed.

Approximately six minutes active elapsed. Foreground Nix ownership released;
no owned command remains running.

## REFACTOR COMPLETE
