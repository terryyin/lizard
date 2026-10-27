# FabioMalpezzi PR review evidence

One-off evidence for the queue frozen on 2026-10-27. This directory retains
measurements and their literal harness; it is not a benchmark service.

Starting target: `2e21e404c1ab9f4e0b49acc80816e816629d0670`.
The manifest identifies exact input bytes independently of changing checkouts.
`affected-inputs.json` supplies the nineteen original example inputs.

Reconstruct the input corpus beside `paired.py`:

```python
import hashlib
import json
from pathlib import Path
import subprocess

root = Path('docs/reviews/fabio-malpezzi-20261027').resolve()
manifest = json.loads((root / 'manifest.json').read_text())
for name, expected in manifest['files'].items():
    if name.startswith('affected/'):
        continue
    data = subprocess.check_output(['git', 'show', manifest['revision'] + ':' + name])
    target = root / 'corpus' / name
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_bytes(data)
    assert hashlib.sha256(data).hexdigest() == expected
for case in json.loads((root / 'affected-inputs.json').read_text()):
    name = 'affected/pr' + str(case['pr']) + '-' + case['filename']
    data = case['source'].encode()
    target = root / 'corpus' / name
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_bytes(data)
    assert hashlib.sha256(data).hexdigest() == manifest['files'][name]
```

Create independent baseline/candidate checkouts at the recorded revisions.
Use the same Nix interpreter for both. From the execution checkout run:

```sh
nix develop -c python docs/reviews/fabio-malpezzi-20261027/paired.py \
  --baseline /absolute/baseline --candidate /absolute/candidate \
  --workload python-ns --out /absolute/result.json
```

Use workload `cli-ns` for the complete fixed-source CLI workload with `-ENS`.
Other reviewed changes select `api` and `cli`. Both workers warm up, batches are
calibrated to at least one second, and baseline/candidate ordering alternates
over twenty pairs. Raw durations, median, standard deviation, paired log-ratio
95% t interval and gate verdict are in each result JSON. An upper interval bound
strictly below3% is required. Absolute paths in original results identify the
actual measured checkouts/interpreter; source revisions and final head identity
are retained with each PR review discussion.

For #527, `527-python-ns.json` and `527-cli-ns.json` compare the frozen starting
target to the production tree in corrected head5067a85. This is both incremental
and cumulative evidence because no earlier reviewed PR had merged. The test-only
refactor after timing did not change production code. Default analysis does not
load the NS extension and its execution path is unchanged.

`controls/` retains an expanded fixed corpus definition and the subsequent
Python NS measurement with all22 nearby regression/control sources. To replay,
reconstruct the original corpus in that directory too, add the UTF-8 sources
from controls.json as `affected/control-<filename>`, and check every hash against
controls/manifest.json. Copy the identical root paired.py into that directory
before invocation, so its fixed corpus root is controls/corpus. The broader
CLI workload is unchanged (only the four original source-tree targets are
analyzed), so its original complete measurement remains applicable.
