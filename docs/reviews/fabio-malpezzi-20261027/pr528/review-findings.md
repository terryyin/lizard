# PR528 review and deferral input

Recommendation: defer this real problem to a linked correction story; leave PR528
unmerged. No product, test, planning, or repository evidence file was changed by
this implementation agent. Only external read-only snapshots and evidence exist.

Reviewed head: b66ec3a7f3ca1b3f59d8a76891ce556406231566.
Source: https://github.com/terryyin/lizard/pull/528
Current target: e871ae448d16893301856b41124aba34496e9536.
Cumulative starting target: 2e21e404c1ab9f4e0b49acc80816e816629d0670.
Metadata/discussion/check snapshot: review-input.json. Discussion empty; authored
head's existing hosted run40309848001 passed, but new boundary probes fail.
Complete proposed patch: proposed-patch.json. No new dependency claim; supersedes
504 after505. No change is assumed correct merely because its CI passed.

## Beneficiary and expected behavior

Users analyzing JavaScript, TypeScript, JSX/TSX and Ruby need regex literals after
return/arrow contexts to retain subsequent conditions and function boundaries.
The reported a/arrow should each have CCN2. Current target reports1; authored
candidate reports2. Ruby `return /#/ =~ s ? 1 : 0` similarly improves1 to2.

Valid division expressions must remain division, preserving their conditions and
tokens. Both the original report and the valid boundary behavior are required.

## Proposed fix's regression

`function divide(obj,a,b,c) { return obj.return / (a || b) / c; }`
must have CCN2. Target: CCN2/token24; authored candidate: CCN1/token18 in js,ts,
jsx,tsx. The candidate's word-boundary keyword lookbehinds interpret the division
slashes as a regex literal and hide the || condition.

The same regression occurs for `obj . return`, `obj. /* member */ return`,
`obj?.return`, valid `$return` identifiers, contextual `of` variables and non-module
`await` identifiers. Ordinary x/myreturn division remains CCN2. Ruby member division
`obj.return / (a || b) / c` regresses CCN2/token22 to1/token16. Ruby split/gsub
regex controls remain unchanged. Template-division controls preserve the following
function's name/span/CCN; interpolation CCN is already opaque and belongs to545.

Public API reproducer:
`nix develop -c python /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr528/reproduce.py --checkout /absolute/checkout`
It calls actual analyze_file.analyze_source_code and compares the function CCN list
with explicit expected values; no parser mocks. Both current target and candidate
exit1 for their different real defects. Complete terminal observations are in
528-reproduction-target.log and528-reproduction-candidate.log. The original
multi-function report and token/boundary probes are also retained in the49inputs.

## Correction estimate and remaining work

Estimated30–45minutes of correction, regression coverage and coherent cleanup,
excluding review/measurement. This exceeds the source's fifteen-minute cutoff.
The existing regex module remains the owner; avoid a second recognizer.

1. Distinguish real expression-start keywords from member names, dollar names and
   contextual identifiers. A fixed word-boundary lookbehind does not know preceding
   member tokens separated by arbitrary whitespace/comments or the for/of context.
2. Keep that lexical decision coherent between js_style_regex_expression and the
   balanced template scanner's imported REGEX_TOKEN. Trace TS/JS, TSX/JSX, Ruby and
   Vue's inherited TypeScript reader; preserve valid division and token boundaries.
3. Cover the new division counterexamples and the original report through public
   analysis, plus existing character/quote/class/flags, following-file NS and
   template boundary guards. Vue-specific performance was not measured separately.
4. Remove the measured affected-input slowdown and remeasure corrected code with
   incremental/cumulative fixed-corpus comparisons. Profile before choosing a
   fast shared context mechanism; do not add unbounded expensive lookbehind work.
5. Independently refactor, run focused/full pytest and make, final hosted CI, then
   publish to the contributor head and merge only under the original strict gates.

## Performance evidence

Raw authored proposal measured diagnostically; these results do not accept its
known incorrect behavior. Root: /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr528.
149fixed files:100original analyzer source inputs plus49related affected cases.
The exact full PR code example appears in all4JS extensions. Other inputs include
13character/escape/quote/flags regex guards in all4extensions, member/division/
contextual identifiers, Ruby controls and balanced-template boundary controls.
No unrelated PR examples enter the affected API workload. inputs.json and
manifest.json identify every byte; the original100source hashes are unchanged.
ManifestSHA256:2b4d99de48defbca46ef3b55568e45edd3f19d7f8366ded49b51e136cbf5d7cc.
Literal unchanged harness: paired.py, SHA256
3064d8d77047db2f084ce9fd6ae09d98acf34453316b7df20fb184cbb3217bf5.

Territory.local arm64,16CPU/48GiB; load14.48/13.09/9.98 before launch, memory48%free.
Other story worktrees' workload unknown; no competing owned heavy commands. Ran
two workloads sequentially, warmed both,20alternating pairs,batches at least1s.
Same Nix Python3.9.11 executable in selected checkout .venv used for both snapshots.

Literal measurement commands (prefix PIP_QUIET=1 was used to suppress setup noise):
`nix develop -c python /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr528/paired.py --baseline /Users/terryyin/.cache/lizard-pr-reviews-20261027/baseline --candidate /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr528-readonly --workload api --out /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr528/528-api.json`
and the identical command with workload cli/out528-cli.json.

| Workload | Median baseline/candidate seconds | SD baseline/candidate | Median slowdown | Paired95%CI | Verdict |
| --- | --- | --- | --- | --- | --- |
| Affected API,84loops | 1.209428/1.444575 | .019069/.025716 | +19.4428% | [+18.9053%,+20.3307%] | FAIL |
| Complete CLI,8loops | 1.313270/1.299958 | .033113/.023542 | -1.0137% | [-1.7114%,+.2591%] | pass |

Current e871 and frozen2e21 differ in product code only by52added NS methods;
default API/CLI do not call that extension. The harness imports NS on both sides,
but its common FunctionInfo initializer patch is unchanged and analysis has no
NS processor. Thus the default-path comparison owns incremental and cumulative
cost for this proposal. No ENS workload verdict is claimed here.

## Bounded unchanged-output profile

`nix develop -c python /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr528/profile-control.py /absolute/checkout /absolute/output.json`
ran once each on baseline and candidate,5000analyses of ordinary division with
no regex literal. Asserted public tuple on both sides:
`('divide',CCN2,token22,parameters4,start1,end1,NLOC1)`.
Total profiled seconds:.983813 ->1.026042. tokens_in_span selftime:.065343 ->.106154,
with180000calls on each side; almost all the extra sampled time occurs there.
That function iterates compiled token-pattern matches. This supports extra lexical
matching cost independent of newly visible tokens; the only production difference
in the compared snapshots is the keyword/arrow REGEX_TOKEN expansion. It is one
diagnostic profile, not a separate confidence-bound performance verdict.
Raw bounded profiles:528-profile-baseline.json,528-profile-candidate.json.

No optimization, code correction, hosted write, PR closure or merge was performed.
The coordinator owns the canonical story/link, durable evidence publication and
final disposition acceptance. All spawned verification commands reached terminal
results; no agent-owned watch or process remains. Nix ownership is released.
