"""Keep public observations and focused snapshot test commands sequential."""
import json
import os
from pathlib import Path
import subprocess
import sys

REPO = Path(__file__).resolve().parents[4]
EVIDENCE = Path(__file__).resolve().parent
ROOT = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr536')
for label in ('target', 'raw', 'adapted'):
    command = [sys.executable, str(EVIDENCE / 'reproduce.py'),
               str(ROOT / label), str(ROOT / 'all-inputs.json')]
    result = subprocess.run(command, cwd='/private/tmp', text=True,
        stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    (ROOT / (label + '-reproduction.log')).write_text(result.stdout)
    print(json.dumps({'label': label, 'command': command,
        'returncode': result.returncode,
        'last_line': result.stdout.splitlines()[-1]}), flush=True)
    files = sorted((ROOT / label / 'test/test_languages').glob('testJavaScript*.py'))
    files += sorted((ROOT / label / 'test/test_languages').glob('testTypeScript*.py'))
    files += [ROOT / label / 'test/test_languages' / name
              for name in ('testES6.py', 'testTSX.py', 'testOptionalOperators.py')]
    command = [sys.executable, '-m', 'pytest', '-q'] + [str(p) for p in files]
    result = subprocess.run(command, cwd=ROOT / label,
        env=dict(os.environ, PYTHONPATH=str(ROOT / label)), text=True,
        stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    (ROOT / (label + '-focused.log')).write_text(result.stdout)
    print(json.dumps({'label': label, 'command': command,
        'returncode': result.returncode,
        'last_line': result.stdout.splitlines()[-1]}), flush=True)
