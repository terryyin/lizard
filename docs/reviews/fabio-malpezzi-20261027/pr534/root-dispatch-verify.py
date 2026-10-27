"""Observe reader delivery through real analysis and unchanged CLI arguments."""
import hashlib
import json
from pathlib import Path
import shutil
import subprocess
import sys

HERE = Path(__file__).resolve().parent
CHECKOUT = HERE.parents[3]
CACHE = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-root-dispatch')
FROZEN = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-dispatch-13c633b8')
BASE = '13c633b8ed1ab5af4a30decfd87ceaeaf7044093'


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


helper = CACHE / 'qualification-verify.py'
shutil.copyfile(HERE / 'qualification-verify.py', helper)
receipts = {}
metrics = {}
for name, snapshot in (('before', FROZEN / 'candidate'), ('working', CHECKOUT)):
    command = [sys.executable, str(helper), str(snapshot),
               str(FROZEN / 'augmented-inputs.json'), '--require-expected']
    result = subprocess.run(command, cwd=CACHE, capture_output=True, text=True)
    log = CACHE / (name + '-metrics.jsonl')
    log.write_text(result.stdout)
    (CACHE / (name + '-metrics.stderr')).write_text(result.stderr)
    receipts[name + '-metrics'] = dict(command=command, returncode=result.returncode,
                                     log=str(log), sha256=digest(log))
    assert result.returncode == 0, (name, result.returncode, result.stderr)
    rows = [json.loads(line) for line in result.stdout.splitlines()]
    assert rows[-1]['cases'] == 166 and not rows[-1]['failures']
    metrics[name] = {row['case']: row['actual'] for row in rows if 'case' in row}
assert len(metrics['before']) == 166
assert metrics['before'] == metrics['working']

source_hashes = json.loads((HERE / 'dispatch-candidate-source-hashes.json').read_text())
assert all(digest(FROZEN / 'candidate' / name) == value
           for name, value in source_hashes.items())
working_hashes = {name: digest(CHECKOUT / name) for name in source_hashes}
different = [name for name, value in source_hashes.items() if working_hashes[name] != value]
assert different == ['lizard_languages/typescript.py'], different
targets = [str(FROZEN / 'original/corpus' / name) for name in (
    'lizard.py', 'lizard_core', 'lizard_languages', 'lizard_ext')]
outputs = {}
for name, snapshot in (('before', FROZEN / 'candidate'), ('working', CHECKOUT)):
    command = [sys.executable, str(snapshot / 'lizard.py')] + targets
    result = subprocess.run(command, cwd=FROZEN / 'original/corpus', capture_output=True)
    log = CACHE / (name + '-cli.txt')
    log.write_bytes(result.stdout)
    receipts[name + '-cli'] = dict(command=command, returncode=result.returncode,
                                 log=str(log), sha256=digest(log))
    assert result.returncode == 1 and not result.stderr, (name, result.returncode, result.stderr)
    outputs[name] = dict(returncode=result.returncode, sha256=digest(log))
assert outputs['before'] == outputs['working']
assert outputs['working']['sha256'] == '88f28b6a85933dd2e1674ca75210fcb891f6511715bcb6a70f3ebbeb39e3e494'
patch = subprocess.check_output(['git', 'diff', '--binary', BASE, '--', 'lizard_languages'],
                                cwd=CHECKOUT)
(CACHE / 'root-dispatch.patch').write_bytes(patch)
payload = dict(base=BASE, working_product='uncommitted; not performance-qualified',
               cases=166, all_seven_fields_equal=True, differing_product_files=different,
               working_product_sha256=working_hashes, cli=outputs, receipts=receipts,
               runtime_patch_sha256=hashlib.sha256(patch).hexdigest())
(HERE / 'root-dispatch-proof.json').write_text(json.dumps(payload, separators=(',', ':')) + '\n')
print(json.dumps(dict(cases=166, all_seven_fields_equal=True, cli=outputs,
                      runtime_patch_sha256=payload['runtime_patch_sha256'])), flush=True)
