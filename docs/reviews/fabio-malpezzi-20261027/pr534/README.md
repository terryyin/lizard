# PR #534 review evidence

The selected SEED-015 technical correction is merged as
[48de7f2](https://github.com/terryyin/lizard/commit/48de7f294831b0b098c9ab2753c831346c645fd8)
and [acknowledged](https://github.com/terryyin/lizard/pull/534#issuecomment-6451652906).
Qualified contributor head0bfc798 retains the originalf407 history. All six
performance gates, full pytest/make and final hosted matrices passed; see
[delivery acceptance](delivery-acceptance.md). Earlier review/timing failures
below are historical, not the corrected candidate's result.

## Original preparation record

Source: https://github.com/terryyin/lizard/pull/534, frozen and refreshed head
`f4074ae860c7999cc37dd081a6f7ebfa777adc83`, base `01afe9c228ba2a3dc5253259d8415d2ef47f0b71`.
Target snapshot: `7186fdbe826bf32b399165cc3d36897a128d4d0d`; fetched trunk
`7ac449f59b3f155fd159ce9aad6794a422045632` has identical analyzer sources.
The full patch, complete PR description/metadata and read-only hosted observations
are retained beside this file. No product correction or hosted write was made.

`inputs.json` contains the complete PR report and authored example, nearby
typed/nested/object controls and relevant inherited #531 examples, across
JS/TS/JSX/TSX/Vue. Its 92 input files and the original 100 broader source files
are frozen by `manifest.json`. No PR533 continuation workload is included.

The external root is
`/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534`.
`freeze.py` archives the target into independent `target`, `raw`, and `child`
snapshots, applies the full patch to raw/child, removes only #531's object
heuristic from child, copies the original fixed source corpus from the parent
cache, writes affected source bytes and recovers the original harness from
immutable commit `af27c910a6a0bff51d8358fde0c37fa9cb059d1a` beside that corpus.
The current central harness has since changed. To rebuild the parent fixed corpus
on another machine, use the central review README's `git show`/hash recipe at
initial revision `2e21e404c1ab9f4e0b49acc80816e816629d0670` and archive that
revision into the parent cache's `baseline` directory. The scripts' absolute
cache root can be changed together for a relocated replay.

Literal commands from the execution checkout, sequential Nix entries:

```sh
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/freeze.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/run-proof.py
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/measure.py
```

`run-proof.py` copies the real `reproduce.py` script into the corpus cache and
launches each snapshot from the corpus directory. The script asserts facade,
registry, function mixin, TS state and test-helper paths before observations.
It compares public `analyze_file` and independently constructed `FileAnalyzer`
results and asserts identical metric lists. Each expected row explicitly checks
name, start/end line and CCN; actual rows also retain NLOC/token count.
The authored/focused suite modes use the same import isolation.

The raw/child focused suites contain both inherited #531 tests and the new #534
test. Child's one failure is the deliberately removed #531 object improvement;
its own #534 authored test passes. These are separate observations.

Supplemental isolation of the nested internal-colon regression was added after
the timed 92-case corpus froze. It is **not** part of the timing claim:

```sh
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/reproduce.py --checkout /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/target --inputs docs/reviews/fabio-malpezzi-20261027/pr534/supplemental-inputs.json
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/reproduce.py --checkout /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/raw --inputs docs/reviews/fabio-malpezzi-20261027/pr534/supplemental-inputs.json
PIP_QUIET=1 nix develop -c python docs/reviews/fabio-malpezzi-20261027/pr534/reproduce.py --checkout /Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/child --inputs docs/reviews/fabio-malpezzi-20261027/pr534/supplemental-inputs.json
```

Replay the retained public CLI observations (expected raw/child defects remain
visible in output; exit 0 alone is not an assertion of correct metrics):

```sh
PIP_QUIET=1 nix develop -c python - <<'PY'
from pathlib import Path
import json, subprocess, sys
here = Path('docs/reviews/fabio-malpezzi-20261027/pr534').resolve()
cache = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534')
case = json.loads((here / 'supplemental-inputs.json').read_text())[0]
(cache / 'supplemental').mkdir(exist_ok=True)
(cache / 'supplemental' / case['filename']).write_text(case['source'])
for snapshot in ('target', 'raw', 'child'):
    for filename in ('reported.js', 'typed_arrows.ts', 'inherited_function_type_string.ts', 'curried_internal_control.js'):
        path = cache / 'corpus/affected' / filename if filename != 'curried_internal_control.js' else cache / 'supplemental' / filename
        command = [sys.executable, str(cache / snapshot / 'lizard.py'), str(path)]
        result = subprocess.run(command, cwd=cache / 'corpus', text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        (cache / (snapshot + '-cli-' + filename + '.log')).write_text(result.stdout)
        assert result.returncode == 0, (command, result.returncode, result.stdout)
        print(snapshot, filename, result.returncode)
PY
```

The benchmark commands expanded by `measure.py` use the same interpreter,
92 immutable affected files, default extensions and identical options, warm
both workers, calibrate 1.5-second batches, and alternate 20 pairs. Raw durations,
medians, SD and paired 95% intervals are retained in `534-api.json` and
`534-cli.json`, along with the terminal logs. These diagnose the invalid raw
proposal; no timing acceptance is claimed. `profile-control.py` asserts all six
public metrics before one 5000-iteration profile on unchanged block-arrow input.

Default-path initial/current equivalence is established by the static analyzer
diff (only unused NS methods differ), 92 actual API outputs and byte-identical
broader CLI outputs. `comparison-basis.json` and baseline/target replay logs own
those observations. The CLI's expected threshold-warning exit is 1; output and
all original logs stay in the external cache. `-ENS` was not measured here.

`*-expected-name-diagnostic.log` and `*-expected-curried-diagnostic.log` retain
the initial review-harness mistakes, with their full expected/actual observations.
`harness-diagnostics.json` explains the corrections without changing any product
assertion. Terminal-log copies have trailing whitespace normalized; original
external logs are retained unchanged. See `review-findings.md` for disposition,
failure causes, consumer boundaries, remaining work and performance conclusions.
