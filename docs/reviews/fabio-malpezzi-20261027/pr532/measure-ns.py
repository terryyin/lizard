"""Cumulative CLI -ENS proof including the accepted Python nesting fix."""
import json
from pathlib import Path
import subprocess
import sys

ROOT = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr532')
command = [sys.executable, str(ROOT / 'affected/paired.py'),
           '--baseline', str(ROOT.parent / 'baseline'),
           '--candidate', str(ROOT / 'corrected'), '--workload', 'cli-ns',
           '--pairs', '20', '--seconds', '8',
           '--out', str(ROOT / 'cumulative-cli-ns.json')]
headroom = subprocess.check_output(['uptime'], text=True).strip()
(ROOT / 'ns-headroom.json').write_text(json.dumps(dict(
    headroom=headroom, command=command,
    reason='Core parameter matching reaches accepted ENS output; prior 1.2s ordinary CLI batches had high variance, so use one 8s cumulative comparison.'
)) + '\n')
print(headroom, flush=True)
with (ROOT / 'cumulative-cli-ns-timings.log').open('w') as log:
    result = subprocess.run(command, cwd='/private/tmp', stdout=log,
                            stderr=subprocess.STDOUT)
assert result.returncode == 0, result.returncode
print((ROOT / 'cumulative-cli-ns-timings.log').read_text().splitlines()[-1])
