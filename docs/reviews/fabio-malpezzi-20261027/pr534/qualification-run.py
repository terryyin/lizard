"""Run one fixed serial qualification, retaining every comparison and signal."""

import hashlib
import json
from pathlib import Path
import shutil
import subprocess
import sys

HERE = Path(__file__).resolve().parent
CACHE = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-performance')


def run(command, log):
    with (CACHE / log).open('w') as output:
        result = subprocess.run(command, cwd=CACHE / 'corpus', stdout=output,
                                stderr=subprocess.STDOUT)
    result.check_returncode()


freeze = json.loads((CACHE / 'freeze.json').read_text())
assert hashlib.sha256((CACHE / 'paired.py').read_bytes()).hexdigest() == freeze['harness_sha256']
assert hashlib.sha256((CACHE / 'manifest.json').read_bytes()).hexdigest() == freeze['original_manifest_sha256']
shutil.copyfile(HERE / 'qualification-verify.py', CACHE / 'qualification-verify.py')
for snapshot in (() if '--measure-only' in sys.argv else ('current', 'frozen', 'candidate')):
    run([sys.executable, str(CACHE / 'qualification-verify.py'),
         str(CACHE / snapshot), str(CACHE / 'inputs.json')], snapshot + '-metrics.jsonl')
    targets = [str(CACHE / 'corpus' / path) for path in (
        'lizard.py', 'lizard_core', 'lizard_languages', 'lizard_ext')]
    command = [sys.executable, str(CACHE / snapshot / 'lizard.py')] + targets
    result = subprocess.run(command, cwd=CACHE / 'corpus', stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE)
    assert result.returncode == 1, (snapshot, result.returncode, result.stderr)
    (CACHE / (snapshot + '-cli-output.txt')).write_bytes(result.stdout)
    print(json.dumps(dict(proof=snapshot, cli_exit=result.returncode,
                          cli_sha256=hashlib.sha256(result.stdout).hexdigest())), flush=True)
print('REUSING_PRE_TIMING_PROOF' if '--measure-only' in sys.argv
      else 'PRE_TIMING_PROOF_COMPLETE', flush=True)
if not any(flag in sys.argv for flag in ('--measure', '--measure-only')):
    raise SystemExit(0)
summary = json.loads((CACHE / 'candidate-metrics.jsonl').read_text().splitlines()[-1])
assert summary['cases'] == 128 and not summary['failures']
for snapshot in ('current', 'frozen', 'candidate'):
    hashes = json.loads((CACHE / (snapshot + '-source-hashes.json')).read_text())
    assert all(hashlib.sha256((CACHE / snapshot / name).read_bytes()).hexdigest() == value
               for name, value in hashes.items())
for baseline in ('current', 'frozen'):
    for workload in ('api', 'cli'):
        label = baseline + '-' + workload
        destination = CACHE / (label + '.json')
        assert not destination.exists(), 'Do not overwrite an earlier measurement'
        command = [sys.executable, str(CACHE / 'paired.py'),
                   '--baseline', str(CACHE / baseline),
                   '--candidate', str(CACHE / 'candidate'),
                   '--workload', workload, '--pairs', '20', '--seconds', '1.5',
                   '--out', str(destination)]
        print(json.dumps(dict(start=label, command=command)), flush=True)
        run(command, label + '-terminal.log')
        result = json.loads(destination.read_text())
        assert len(result['raw']) == 20
        assert min(value for pair in result['raw'] for value in pair.values()) >= 1
        print(json.dumps(dict(finished=label, **{key: result[key] for key in (
            'medians', 'stdev', 'median_slowdown_pct',
            'paired_geometric_slowdown_pct', 'paired_95pct_ci', 'passes')})), flush=True)
