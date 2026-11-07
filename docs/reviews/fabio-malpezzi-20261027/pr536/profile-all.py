"""Profile a frozen unchanged-output subset, once for each source snapshot."""
import json
import os
from pathlib import Path
import subprocess
import sys

EVIDENCE = Path(__file__).resolve().parent
ROOT = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr536')
observations = {}
for label in ('parent', 'raw', 'target', 'adapted'):
    command = [sys.executable, str(EVIDENCE / 'profile-proposal.py'), str(ROOT / label)]
    result = subprocess.run(command, cwd='/private/tmp',
        env=dict(os.environ, PYTHONPATH=''), text=True,
        stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    (ROOT / (label + '-profile.log')).write_text(result.stdout)
    assert result.returncode == 0, result.stdout
    observations[label] = json.loads(result.stdout.splitlines()[0])
    print(json.dumps({'label': label, 'command': command,
                      'observation': observations[label]}), flush=True)
for before, after in [('parent', 'raw'), ('target', 'adapted')]:
    assert observations[before]['unchanged_output'] == observations[after]['unchanged_output']
(ROOT / 'profile-invariants.json').write_text(json.dumps(observations) + '\n')
print('Both before/after pairs have identical valid named-signature outputs.')
