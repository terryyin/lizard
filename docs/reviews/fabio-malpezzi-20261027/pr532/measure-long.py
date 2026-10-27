"""One longer final CLI experiment after variance and profile diagnosis."""
import json
from pathlib import Path
import subprocess
import sys

ROOT = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr532')
REPO = Path(__file__).resolve().parents[4]
command = [sys.executable, str(ROOT / 'affected/paired.py'),
           '--baseline', str(ROOT / 'target'),
           '--candidate', str(ROOT / 'corrected'), '--workload', 'cli',
           '--pairs', '20', '--seconds', '8',
           '--out', str(ROOT / 'corrected-cli-long.json')]
headroom = subprocess.check_output(['uptime'], text=True).strip()
(ROOT / 'long-headroom.json').write_text(json.dumps(dict(
    headroom=headroom, command=command,
    reason='Prior 1.2s CLI batches had high variance; identical output and one bounded profile show no material parameter-matcher cost.'
)) + '\n')
print(headroom, flush=True)
with (ROOT / 'corrected-cli-long-timings.log').open('w') as log:
    result = subprocess.run(command, cwd='/private/tmp', stdout=log,
                            stderr=subprocess.STDOUT)
assert result.returncode == 0, result.returncode
print((ROOT / 'corrected-cli-long-timings.log').read_text().splitlines()[-1])
