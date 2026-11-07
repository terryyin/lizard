"""Sequence snapshot observations and focused tests without Nix overlap."""
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys

REPO = Path(__file__).resolve().parents[4]
EVIDENCE = Path(__file__).resolve().parent
ROOT = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr535')
phase = sys.argv[1]
if phase == 'initial':
    for label in ('target', 'raw', 'raw-child'):
        path = ROOT / (label + '-reproduction.log')
        if path.exists() and not (ROOT / (label + '-first-fixture.log')).exists():
            shutil.copyfile(path, ROOT / (label + '-first-fixture.log'))
    subprocess.run([sys.executable, str(EVIDENCE / 'bootstrap.py')], check=True)
    labels = ['target', 'raw', 'raw-child']
else:
    labels = ['corrected']
    if not (ROOT / 'corrected').exists():
        shutil.copytree(ROOT / 'target', ROOT / 'corrected')
    for name in ('lizard_languages/javascript_functions.py',
                 'test/test_languages/testTypeScriptThisParameters.py'):
        shutil.copyfile(REPO / name, ROOT / 'corrected' / name)

for label in labels:
    result = subprocess.run([sys.executable, str(EVIDENCE / 'reproduce.py'),
        str(ROOT / label), str(ROOT / 'all-inputs.json')], cwd='/private/tmp',
        text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    (ROOT / (label + '-reproduction.log')).write_text(result.stdout)
    print(json.dumps({'label': label, 'returncode': result.returncode,
                      'result': result.stdout.splitlines()[-1]}), flush=True)

if phase != 'initial':
    testfile = 'test/test_languages/testTypeScriptThisParameters.py'
    for label in ('target', 'raw-child', 'corrected'):
        shutil.copyfile(REPO / testfile, ROOT / label / testfile)
        command = [sys.executable, '-m', 'pytest', '-q', testfile]
        result = subprocess.run(command, cwd=str(ROOT / label),
            env=dict(os.environ, PYTHONPATH=str(ROOT / label)), text=True,
            stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        (ROOT / (label + '-regression.log')).write_text(result.stdout)
        print(json.dumps({'label': label, 'command': command,
            'returncode': result.returncode,
            'result': result.stdout.splitlines()[-1]}), flush=True)
