"""Measure combined CLI -ENS startup and analysis on the original broad bytes."""
import json
from pathlib import Path
import subprocess
import sys

ROOT = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr535')
headroom = {name: subprocess.check_output(command, text=True) for name, command in
    [('uptime', ['uptime']), ('memory', ['vm_stat']),
     ('logical_cpus', ['sysctl', '-n', 'hw.logicalcpu'])]}
decision = {'host': 'Territory.local', 'headroom': headroom,
    'workload': '20 alternating calibrated >=1.2s cumulative cli-ns pairs',
    'expected_cost': 'about one minute, following the six terminal paired runs',
    'concurrency': 'one measured CLI at a time; coordinator keeps owned host-heavy work idle',
    'choice': 'run sequentially; retain interval and reconsider if confidence crosses3% or actual batches fall below1s',
    'owner': 'review_535 implementation agent'}
(ROOT / 'cumulative-cli-ns-decision.json').write_text(json.dumps(decision, indent=2) + '\n')
print(json.dumps(decision), flush=True)
command = [sys.executable, str(ROOT / 'affected/paired.py'),
    '--baseline', str(ROOT.parent / 'baseline'),
    '--candidate', str(ROOT / 'corrected'), '--workload', 'cli-ns',
    '--pairs', '20', '--seconds', '1.2',
    '--out', str(ROOT / 'cumulative-cli-ns.json')]
with (ROOT / 'cumulative-cli-ns-timings.log').open('w') as log:
    result = subprocess.run(command, cwd='/private/tmp', stdout=log,
                            stderr=subprocess.STDOUT)
assert result.returncode == 0
data = json.loads((ROOT / 'cumulative-cli-ns.json').read_text())
print(json.dumps({key: data[key] for key in
    ('workload', 'loops', 'medians', 'paired_95pct_ci', 'passes')}), flush=True)
