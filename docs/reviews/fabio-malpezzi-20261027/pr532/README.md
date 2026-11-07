# PR #532 correction evidence

Merged as9bc44563aad42b780ff0264aa5a3a972a583b423, corrected head2a8af8a.
Final fourteen-job CI, exact validated merge tree and all timing gates pass.
Thank-you: https://github.com/terryyin/lizard/pull/532#issuecomment-6436143243.

Frozen head `d0713bf5b4dcf4bf24453156a3756688627abbc0`, base
`01afe9c228ba2a3dc5253259d8415d2ef47f0b71`, current review target
`33807306953535ebda5c0fa42797b1e5e7ef73d1`. The full original patch,
metadata, empty discussion/reviews and historical checks are retained here.
The raw proposal fixes the report but loses valid destructured bindings and
omits several dollar identifiers. The merged correction addresses those
through existing reader/binding/core owners. See review-findings.md for gates.

The external root is `/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr532`.
`bootstrap.py` reconstructs the immutable target/raw/current-target-plus-raw
snapshots and the original 100 broad source files from Git objects. It creates
no Git worktree or branch. The target plus raw snapshot replaces only the
function reader with the authored version. `prepare-proof.py` copies the five
corrected production files into a target copy, freezes the 16 affected and
21 expanded core-consumer source files, and retains every hash.

Sequential commands from the selected execution checkout:

```sh
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr532/bootstrap.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr532/prepare-proof.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr532/run-proof.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr532/measure.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr532/profile.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr532/measure-long.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr532/measure-ns.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr532/verify-replay.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr532/retain-evidence.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr532/finish-proof.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr532/final-cli.py
```

The one central harness adds `api-parameters`, which reads public normalized
names and counts after each actual analysis. Its measured SHA256 is
`ec1c9c5eac0ad6a29294ecbd987b2a55d4cf31db145486d065bf13c096b76520`.
Existing modes retain their behavior. Historical result replay recovers the
original harness from immutable commit af27c910 as described in the parent
README; no duplicate implementation is checked in.

The real `reproduce.py` script asserts facade, registry, JS functions, TS states,
FunctionInfo and test-helper paths before every public observation. Its expected
counts come from valid source semantics; the reports expect render=3, round=2.
`supplemental.json` retains the pre-existing PHP multi-parameter count and the
separate TypeScript this declaration. `preexisting.json` retains the unrelated
generic function declaration not recognized on either starting or raw code.
Neither is silently converted into a passing expectation.

Timings use one Python 3.9.11 interpreter, default extensions, fixed bytes,
warmups, calibration and 20 alternating pairs. The first five batches target
1.2 seconds; the one justified corrected CLI retry targets eight seconds after
the high-variance result and unchanged-output profile. Every raw duration,
median, SD and paired 95% interval remains in its JSON. The cumulative `cli-ns`
comparison includes the accepted Python nesting change and core matcher.
After host load and the subsequent ENS variance fell, root selected a fixed
40-pair ordinary CLI run. Its prospective decision and unchanged-code/corpus
assertions are retained; earlier inconclusive results keep their verdicts.

Profiler setup initially failed because the snapshot's legacy Python 2
`profile.py` shadowed the standard library. The fixed command starts cProfile
from `/private/tmp`, retains the real analyzer argv, asserts executed source
paths, and checks identical complete CLI output. A single profile cannot
attribute host scheduling noise or establish a timing gate.

Terminal copies are whitespace-normalized and split at 230 lines. Original
external logs, binary profiles and broad outputs remain untouched. Read the
supplemental consumer trace for historical replay and newly fetched website
consumers; it supersedes earlier static-record dispositions for those paths.
website-consumer.json preserves the first missing-runtime attempt as observed:false.
Root resolved that gap in /private/tmp/lizard-pr532-validation on current trunk:
its Nix setup installs Flask, then validate-current-base.py proves the exact
anticipated merge tree with full pytest1751/7, make and four real HTTP cases.
Run that helper through Nix from the validation worktree; it verifies its base
and copies the recorded corrected files and original authored tests before proof.
current-base-proof.json and website-integrated.log retain the successful evidence.
