"""Retain unchanged public metrics, broad CLI output and diagnostic attribution."""

import hashlib
import json
from pathlib import Path
import subprocess
import sys

HERE = Path(__file__).resolve().parent
CHECKOUT = HERE.parents[3]
CACHE = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-context')
FROZEN = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-optimized-8dac7b80')
BASE = '8dac7b80c2478d65b54d8ec96d6aa36bf4530b91'


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def metrics(path):
    rows = [json.loads(line) for line in path.read_text().splitlines()]
    assert rows[-1]['cases'] == 128 and not rows[-1]['failures']
    return {row['case']: row['actual'] for row in rows if 'case' in row}


before = json.loads((CACHE / 'candidate-profile.json').read_text())
after = metrics(CACHE / 'final-metrics.jsonl')
assert len(before['metrics']) == len(after) == 128
assert before['metrics'] == after
profiles = {name: json.loads((CACHE / (name + '-profile.json')).read_text())
            for name in ('current', 'candidate', 'working', 'working-demand')}
assert profiles['candidate']['metrics'] == profiles['working']['metrics'] == after
assert profiles['working-demand']['metrics'] == after
for snapshot in ('candidate', 'current', 'frozen'):
    hashes = json.loads((FROZEN / (snapshot + '-source-hashes.json')).read_text())
    assert all(digest(FROZEN / snapshot / name) == value for name, value in hashes.items())

targets = [str(FROZEN / 'corpus' / name) for name in (
    'lizard.py', 'lizard_core', 'lizard_languages', 'lizard_ext')]
outputs = {}
for name, root in (('base', FROZEN / 'candidate'), ('working', CHECKOUT)):
    command = [sys.executable, str(root / 'lizard.py')] + targets
    result = subprocess.run(command, cwd=FROZEN / 'corpus', capture_output=True)
    assert result.returncode == 1, (name, result.returncode, result.stderr)
    (CACHE / (name + '-cli.txt')).write_bytes(result.stdout)
    outputs[name] = dict(exit=result.returncode,
                         sha256=hashlib.sha256(result.stdout).hexdigest())
assert outputs['base'] == outputs['working']
assert outputs['base']['sha256'] == '49e9a7922e48f71ac1702f2da38939d93c24b4339d0d3618dfdfdebe1287150b'
patch = subprocess.check_output(['git', 'diff', '--binary', BASE, '--', 'lizard_languages'],
                                cwd=CHECKOUT)
(CACHE / 'context.patch').write_bytes(patch)
selected = ('preprocess', 'return_annotation_context', 'return_type_colon',
            '_has_branch_separators', '_state_global')
payload = dict(base=BASE, candidate='uncommitted; not throughput-qualified', cases=128,
               fields=before['fields'], public_metrics_equal=True, cli=outputs,
               patch_sha256=hashlib.sha256(patch).hexdigest(),
               profiles={name: dict(total_seconds=value['total_seconds'],
                                    context=value['context'],
                                    selected=[row for row in value['rows']
                                              if row['function'] in selected])
                         for name, value in profiles.items()},
               evidence_sha256={name: digest(CACHE / name) for name in (
                   'current-profile.json', 'candidate-profile.json',
                   'working-profile.json', 'working-demand-profile.json',
                   'final-metrics.jsonl')})
(HERE / 'context-proof.json').write_text(json.dumps(payload, separators=(',', ':')) + '\n')
print(json.dumps(dict(cases=128, all_seven_fields_equal=True, cli=outputs,
                      patch_sha256=payload['patch_sha256'],
                      candidate=payload['candidate'])), flush=True)
