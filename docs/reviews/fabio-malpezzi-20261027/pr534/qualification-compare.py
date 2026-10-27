"""Compare an uncommitted optimization with the qualified semantic snapshot."""

import hashlib
import json
from pathlib import Path
import subprocess

HERE = Path(__file__).resolve().parent
CHECKOUT = HERE.parents[3]
CACHE = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-performance')


def metrics(filename):
    rows = [json.loads(line) for line in (CACHE / filename).read_text().splitlines()]
    assert rows[-1]['cases'] == 128 and not rows[-1]['failures']
    return {row['case']: row['actual'] for row in rows if 'case' in row}


before = metrics('candidate-metrics.jsonl')
after = metrics('optimization-metrics.jsonl')
assert len(before) == len(after) == 128
assert before == after, [name for name in before if before[name] != after.get(name)]
profiles = [json.loads((CACHE / (name + '-profile.json')).read_text())
            for name in ('current', 'candidate')]
assert profiles[0]['actual'] == profiles[1]['actual'] == before['block_branches.js']
patch = subprocess.check_output([
    'git', 'diff', '--binary', '74bc899db957d27004f9fd58b095e7394fba93dd',
    '--', 'lizard_languages'], cwd=CHECKOUT)
(CACHE / 'optimization.patch').write_bytes(patch)
payload = dict(base='74bc899db957d27004f9fd58b095e7394fba93dd',
               candidate='uncommitted optimization; not timed', cases=128,
               fields=['name', 'start_line', 'end_line', 'cyclomatic_complexity',
                       'nloc', 'token_count', 'parameter_count'],
               patch_sha256=hashlib.sha256(patch).hexdigest(),
               profile_full_metrics_match=True)
(CACHE / 'optimization-proof.json').write_text(json.dumps(payload, indent=2) + '\n')
print(json.dumps(payload))
