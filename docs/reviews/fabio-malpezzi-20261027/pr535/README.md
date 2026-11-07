# PR535 receiver declaration review

Merged d7be8b90ac5872faf86e329e7d7249c98029f045 at corrected head
1a44c0cad59ea3f3dcabe3c1ac601d905437a7c7 after independent refactor, full/HTTP,
all performance gates and fourteen-job CI. Thank-you:
https://github.com/terryyin/lizard/pull/535#issuecomment-6436990416.

Frozen initial contributor head: `171079e625966ac9c17aee29a5d74256befd85ae`.
Its parent is the original PR532 head `d0713bf5b4dcf4bf24453156a3756688627abbc0`.
Connector-reported original PR base: `01afe9c228ba2a3dc5253259d8415d2ef47f0b71`.
Fresh Git target: `3a11bc6a5452be40a32721b5f6e2ab2fa787ca83`; it differs
from corrected PR532 merge `9bc4456` only in five planning/profile files.
Starting cumulative baseline remains `2e21e404c1ab9f4e0b49acc80816e816629d0670`.
The selected execution checkout is `c3ca2c73`; product sources match the target.

The complete Git diff and hosted patch/metadata/discussion/reviews/threads are
retained. The discussion, reviews and threads are empty. Historical run40418659485
passes four old matrix jobs; it does not prove the current Ubuntu/Windows ×
Python3.8–3.14 requirement, copied in required-workflow.yml.
literal-git-patches.json preserves exact Git diff strings; displayed .diff copies
normalize context-line whitespace to satisfy the repository evidence guard.
Each display file is labelled DISPLAY ONLY. No helper reads or applies these
copies: bootstrap reconstructs from immutable Git source bytes; any patch replay
must extract the exact escaped string from JSON or rerun the recorded git diff.
Older executable evidence helpers are reused under their recorded immutable
revision (PR532 at c3ca2c73), with af27c910's historical harness for pre532 runs.
Their prepare/copy helpers must not be rerun against a later checkout's product
sources. Existing recipes and external originals remain unchanged.

The reported `onClick` has one runtime parameter. The official TypeScript
[function handbook](https://www.typescriptlang.org/docs/handbook/functions#this-parameters)
defines a leading fake `this` parameter. The current target reports two; the
full raw head and child-only proposal report one. The defect is useful to fix
because it creates false parameter warnings and incorrect API/output counts.

The raw full head carries superseded PR532 code and fails accepted binding and
dollar controls. `raw-child` applies only the two PR535 reader changes onto
the current corrected target, retaining its root-only annotation colon guard.
It fixes receivers but mistakes a nested property `this` after a comma for a
receiver, changing public parameters from `['receiver','tail']` to
`['first','tail']` for `function unpack({first, this: receiver}, tail) {}`.
Both the original and adapted child patches are retained for this distinction.

Before product edits, correction plus cleanup was estimated at10minutes.
The three-line correction and new focused file fit that estimate; no upward
revision was needed. It reuses JavaScriptBindings.at_start and the existing
annotation state. There is no new parameter state or core metric change.
ADR-0001 (`docs/adrs/0001-ubiquitous-language-accepted.md`) governs metric
meanings; ADR-0000 preserves human architectural ownership. No conflict arose.

The external root is `/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr535`.
bootstrap.py reconstructs target/raw/parent snapshots from trusted Git archives,
adapts the child only, retains the exact existing21-input PR532 corpus, and adds
13receiver/boundary files. The affected API corpus has29JS-family files; the
parameter-consumer corpus has34files including five other-language controls.
Every broad run uses the same original100Python source files, never the changing
checkout. The central paired harness remains SHA256
`ec1c9c5eac0ad6a29294ecbd987b2a55d4cf31db145486d065bf13c096b76520`.

Literal sequential runtime commands from the selected checkout:

```sh
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr535/run-proof.py initial
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr535/run-proof.py corrected
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr535/focused.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr535/measure.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr535/measure-ns.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr535/final-cli.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr535/finish-proof.py
```

Initial fixture setup incorrectly expected class method `Reader::read`; actual
project naming is `read` on target, raw and child. The first-fixture logs retain
that failure. Only this name expectation was aligned; its expected count and
source stayed unchanged. The second initial run uses the valid convention.
There were no failed import/runtime setups. Nix printed an existing platformdirs
dependency warning but exited successfully. Every subprocess is a real script
outside execution cwd, with the intended snapshot first in sys.path; reproduce.py
asserts facade, registry, JS function reader, TS states and FunctionInfo paths
before and after collection. Pytest runs in each snapshot with explicit PYTHONPATH.

The new28-test file fails11cases on target, four on child-only, and passes28on
corrected code. Public34-case observations pass fully on corrected code and
retain all accepted PR532 assertions. Unrelated PHP multi-parameter and generic
declaration diagnostics remain in PR532's unchanged supplemental records;
neither fixture was silently edited to hide those defects.

The focused complete reader/extension/analyzer/CLI/options group passes628/5.
The final helper runs the additional OptionalOperators and registry consumers,
fills the installed traces, asserts unchanged non-parameter metrics, checks
complete current broad CLI and -ENS output equality, verifies original broad
hashes, and copies terminal evidence with whitespace-normalized EOF. Original
external logs remain intact; long retained copies split at230lines.
The supplemental consumers pass44tests. JSON records exceeding230lines are
retained as valid compact JSON so every raw pair remains directly parseable.

Timings use Python3.9.11 on Territory.local, default extensions, warmed calibrated
batches and20alternating pairs. The machine was inspected before proof and each
workload; only one measured process is active at a time and the coordinator kept
owned heavy work idle. Initial load5.90 and free/purgeable/inactive memory were
observed before the run choice; per-workload raw headroom is retained. Variance
or an interval reaching3% keeps its gate open and requires reconsideration.
The cumulative CLI -ENS run has its own fresh headroom/cost decision and directly
measures module startup. The prior Python NS API observation is reused only for
its unchanged Python reader, FunctionInfo and NS extension execution paths.

Git commands were read-only: status/diff/log/show, and
`git fetch origin master pull/535/head` refreshed current target3a11. No staging,
branch switch, stash, observer change, commit, push, comment or merge occurred.
The two coordinator-owned pending metadata edits were preserved.

Coordinator current-base proof: validate-current-base.py ran through Nix from
/private/tmp/lizard-pr532-validation on3a11bc6, with current Flask dependencies.
It copies the frozen corrected reader/new tests and preserves the authored test.
Full pytest1780/7 and make pass. The real /analyze endpoint observes onClick2
before correction and1 afterward; receiver-only0, accepted render3 and binding2
controls also pass. The JSON records asserted executed paths and anticipated
merge tree cf67b36f7ecb3f066d72727ff9a1a5db1c6afaff; logs preserve all results.

Independent refactor_535 returned REFACTOR COMPLETE with no edits or proof changes.
One failed read-only initialization came from overlapping Nix shellHooks; a final
sequential initialization exited0 with Python/pytest imports verified. It supplies
no replacement for the accepted runtime proof, which predates the race and used
the independent validation environment for full/HTTP checks.

Publication preserved contributor history and merged the corrected532 dependency.
The corrected candidate tree exactly matches the full-tested cf67b36f tree.
Current trunk95d65b2 adds only planning changes; validation fast-forward reused
unchanged runtime evidence and produced c4a174edc00c146aec4b457d403611f0f003633d,
exactly the hosted and landed tree. CI40472106878 succeeded on all14 jobs.
Final head/base refresh preceded guarded merge; thanks followed confirmation.
hosted-publication.json and final-ci.json retain exact revisions and job states.
