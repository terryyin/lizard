"""One prospectively fixed longer run for the inconclusive cumulative CLI."""
import hashlib
import json
import math
import os
from pathlib import Path
import statistics
import subprocess
import sys

HERE = Path(__file__).resolve().parent
REPO = HERE.parents[3]
ROOT = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr535')
for row in json.loads((ROOT / 'snapshot-hashes.json').read_text()):
    for label in ('baseline', 'target', 'corrected'):
        folder = ROOT.parent / 'baseline' if label == 'baseline' else ROOT / label
        assert hashlib.sha256((folder / row['path']).read_bytes()).hexdigest() == row[label]
    assert hashlib.sha256((REPO / row['path']).read_bytes()).hexdigest() == row['corrected']
harness = ROOT / 'affected/paired.py'
assert harness.read_bytes() == (HERE.parent / 'paired.py').read_bytes()
for name, expected in json.loads((ROOT / 'affected/manifest.json').read_text()).items():
    assert hashlib.sha256((ROOT / 'affected/corpus' / name).read_bytes()).hexdigest() == expected
prior = json.loads((ROOT / 'cumulative-cli.json').read_text())
ens = json.loads((ROOT / 'cumulative-cli-ns.json').read_text())
command = [sys.executable, str(harness), '--baseline', str(ROOT.parent / 'baseline'),
    '--candidate', str(ROOT / 'corrected'), '--workload', 'cli',
    '--pairs', '40', '--seconds', '4',
    '--out', str(ROOT / 'cumulative-cli-long.json')]
decision = {'owner': 'review_535', 'host': os.uname().nodename,
    'cpus': os.cpu_count(), 'launch_load': os.getloadavg(),
    'memory': subprocess.check_output(['vm_stat'], text=True), 'command': command,
    'prior_interval': prior['paired_95pct_ci'],
    'prior_log_ratio_sd_pct': 100 * statistics.stdev(
        math.log(r['candidate'] / r['baseline']) for r in prior['raw']),
    'new_evidence': 'Terminal ENS upper1.2723% at load8.99 versus ordinary CLI upper3.2665% at load11.58. This supports a quieter-window/longer-batch decision, not cross-workload equivalence or a pass for the original ordinary run.',
    'choice': 'One fixed40-pair4-second calibrated run, about six minutes, one active subprocess at a time. Coordinator keeps owned heavy work idle. Judge only its terminal interval; if unresolved leave gate open.',
    'harness_sha256': hashlib.sha256(harness.read_bytes()).hexdigest(),
    'gate': 'Upper paired95% slowdown bound strictly below3%; retain every pair and earlier inconclusive result.'}
(ROOT / 'final-cli-decision.json').write_text(json.dumps(decision, indent=2) + '\n')
print(json.dumps(decision), flush=True)
with (ROOT / 'cumulative-cli-long-timings.log').open('w') as log:
    result = subprocess.run(command, cwd='/private/tmp', stdout=log,
                            stderr=subprocess.STDOUT)
assert result.returncode == 0
data = json.loads((ROOT / 'cumulative-cli-long.json').read_text())
print(json.dumps({key: data[key] for key in ('loops', 'medians', 'stdev',
    'median_slowdown_pct', 'paired_geometric_slowdown_pct',
    'paired_95pct_ci', 'passes')}), flush=True)
