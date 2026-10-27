"""One serial Nix entry owns semantic prerequisites and the eight strict gates."""
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys
import time

ROOT = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr536-corrected')
CHECKOUT = Path('/Users/terryyin/git/lizard/.worktrees/report-consistent-javascript-and-typescript-arro')
CANDIDATE = 'candidate-5'


def manifest(path, product=False):
    result = {}
    for file in sorted(path.rglob('*')):
        name = str(file.relative_to(path))
        if not file.is_file() or '__pycache__' in file.parts or file.suffix == '.pyc':
            continue
        if product and not (name == 'lizard.py' or name.startswith(
                ('lizard_core/', 'lizard_languages/', 'lizard_ext/'))):
            continue
        result[name] = hashlib.sha256(file.read_bytes()).hexdigest()
    return result


def invariant():
    corpus = json.loads((ROOT / 'proof/manifest.json').read_text())
    assert manifest(ROOT / 'proof/corpus') == corpus
    for label in ['baseline', 'target', CANDIDATE]:
        expected = json.loads((ROOT / (label + '-product.json')).read_text())
        assert manifest(ROOT / label, product=True) == expected
    assert manifest(CHECKOUT, product=True) == json.loads(
        (ROOT / (CANDIDATE + '-product.json')).read_text())


def host(phase):
    return {'phase': phase, 'time': time.time(), 'load': os.getloadavg(),
            'cores': os.cpu_count(), 'python': sys.version,
            'executable': sys.executable,
            'memory': subprocess.check_output(['vm_stat'], text=True),
            'swap': subprocess.check_output(['sysctl', 'vm.swapusage'], text=True)}


def run(command, log):
    assert not log.exists()
    with log.open('w') as stream:
        result = subprocess.run(command, cwd='/private/tmp', stdout=stream,
                                stderr=subprocess.STDOUT)
    assert result.returncode == 0, (command, result.returncode)


invariant()
run([sys.executable, '/private/tmp/lizard-corrected-verify.py'],
    ROOT / (CANDIDATE + '-semantic.log'))
phases = [('target', 'api-parameters'), ('target', 'api'),
          ('baseline', 'api-parameters'), ('baseline', 'api'),
          ('target', 'cli'), ('target', 'cli-ns'),
          ('baseline', 'cli'), ('baseline', 'cli-ns')]
for comparison, workload in phases:
    name = CANDIDATE + '-' + comparison + '-' + workload
    evidence = [host('before')]
    (ROOT / (name + '-host.json')).write_text(json.dumps(evidence) + '\n')
    command = [sys.executable, '/private/tmp/lizard-corrected-paired.py',
               '--baseline', str(ROOT / comparison), '--candidate', str(ROOT / CANDIDATE),
               '--workload', workload, '--pairs', '20', '--seconds',
               '4' if workload.startswith('cli') else '2',
               '--out', str(ROOT / (name + '.json'))]
    print(json.dumps({'launch': command, 'load': os.getloadavg()}), flush=True)
    run(command, ROOT / (name + '.log'))
    evidence.append(host('after'))
    (ROOT / (name + '-host.json')).write_text(json.dumps(evidence) + '\n')
    result = json.loads((ROOT / (name + '.json')).read_text())
    minimum = min(value for row in result['raw'] for value in row.values())
    print(json.dumps({'name': name, 'interval': result['paired_95pct_ci'],
                      'minimum_batch': minimum, 'passes': result['passes']}), flush=True)
    invariant()
    if not result['passes'] or minimum <= 1:
        print('FAIL: remaining gates unlaunched; retain all raw evidence.', flush=True)
        sys.exit(3)
print('PASS: all eight strict incremental/cumulative gates; immutable sources/corpus verified.', flush=True)
