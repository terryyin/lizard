"""Run isolated snapshot observations; nonzero observations are retained."""
import json
from pathlib import Path
import shutil
import subprocess
import sys

HERE = Path(__file__).resolve().parent
CACHE = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534')
for name in ('reproduce.py', 'authored-tests.py', 'inputs.json'):
    shutil.copy2(HERE / name, CACHE / name)
for snapshot in ('target', 'raw', 'child', 'baseline'):
    checkout = (CACHE / snapshot if snapshot != 'baseline' else CACHE.parent / 'baseline')
    command = [sys.executable, str(CACHE / 'reproduce.py'), '--checkout', str(checkout)]
    result = subprocess.run(command, cwd=CACHE / 'corpus', text=True,
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    (CACHE / (snapshot + '-reproduction.log')).write_text(result.stdout)
    rows = [json.loads(line) for line in result.stdout.splitlines()]
    print(json.dumps(dict(snapshot=snapshot, exit=result.returncode, summary=rows[-1])))
if '--observations-only' in sys.argv:
    raise SystemExit(0)
for snapshot in ('target', 'raw', 'child'):
    for suite in ('authored', 'focused'):
        command = [sys.executable, str(CACHE / 'reproduce.py'), '--checkout',
                   str(CACHE / snapshot), '--suite', suite]
        result = subprocess.run(command, cwd=CACHE / 'corpus', text=True,
                                stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        (CACHE / (snapshot + '-' + suite + '.log')).write_text(result.stdout)
        print(snapshot, suite, result.returncode, result.stdout.splitlines()[-1])

def actual(name):
    return [json.loads(line)['actual'] for line in
            (CACHE / (name + '-reproduction.log')).read_text().splitlines()[1:-1]]

targets = [str(CACHE / 'corpus' / name) for name in
           ('lizard.py', 'lizard_core', 'lizard_languages', 'lizard_ext')]
outputs = {}
for name in ('target', 'raw', 'baseline'):
    checkout = CACHE / name if name != 'baseline' else CACHE.parent / 'baseline'
    result = subprocess.run([sys.executable, str(checkout / 'lizard.py')] + targets,
                            cwd=CACHE / 'corpus', stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT)
    outputs[name] = result.stdout
    (CACHE / (name + '-broad-cli.log')).write_bytes(result.stdout)
    print(name, 'broad-cli', result.returncode)
import hashlib
comparison = dict(default_api_cases=len(actual('target')),
                  target_initial_actual_equal=actual('target') == actual('baseline'),
                  broad_cli_target_initial_equal=outputs['target'] == outputs['baseline'],
                  broad_cli_target_raw_equal=outputs['target'] == outputs['raw'],
                  cli_sha256=hashlib.sha256(outputs['target']).hexdigest(),
                  ens_measured=False)
assert comparison['target_initial_actual_equal'], comparison
assert comparison['broad_cli_target_initial_equal'], comparison
assert comparison['broad_cli_target_raw_equal'], comparison
(CACHE / 'comparison-basis.json').write_text(json.dumps(comparison, indent=2) + '\n')
print(comparison)
