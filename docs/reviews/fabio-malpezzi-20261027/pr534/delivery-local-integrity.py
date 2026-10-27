"""Retain final local-proof receipts and verify unchanged delivery content."""
import ast
from datetime import datetime, timezone
import hashlib
import json
from pathlib import Path
import re
import subprocess
import sys

HERE = Path(__file__).resolve().parent
CHECKOUT = HERE.parents[3]
CACHE = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-delivery-68a1ce8')
QUALIFIED = CACHE.parent / 'seed015-root-737aeaf7'


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def git(*args):
    return subprocess.check_output(['git'] + list(args), cwd=CHECKOUT)


proof = json.loads((HERE / 'delivery-local-verification.json').read_text())
assert git('rev-parse', 'HEAD').decode().strip() == proof['head']
assert git('rev-parse', 'MERGE_HEAD').decode().strip() == proof['merge_head']
assert not git('diff', '--name-only', '--diff-filter=U')
paths = ('lizard.py', 'lizard_core', 'lizard_languages', 'lizard_ext', 'test')
assert not git('diff', '--name-only', proof['qualified_revision'], '--', *paths)
assert all(digest(CHECKOUT / name) == value
           for name, value in proof['actual_product_hashes'].items())
assert len(proof['actual_product_hashes']) == 104
manifest = Path(proof['test_hash_manifest'])
assert digest(manifest) == proof['test_hash_manifest_sha256']
tests = json.loads(manifest.read_text())
assert len(tests) == proof['test_files'] == 159
assert all(digest(CHECKOUT / name) == value for name, value in tests.items())
assert sys.version == proof['python'] and sys.executable == proof['executable']
receipts = {}
for name in ('full-pytest', 'make'):
    receipt_path = CACHE / (name + '-receipt.json')
    receipt = json.loads(receipt_path.read_text())
    log = Path(receipt['log'])
    assert receipt['returncode'] == 0 and digest(log) == receipt['log_sha256']
    text = log.read_text()
    match = re.search(r'2118 passed, 7 skipped, (\d+) warnings in ([0-9.]+)s', text)
    assert match, name
    receipt.update(passed=2118, skipped=7, warnings=int(match.group(1)),
                   seconds=float(match.group(2)), receipt_sha256=digest(receipt_path))
    if name == 'make':
        assert 'TOTAL' in text and '94%' in text
        assert 'pycodestyle lizard.py lizard_ext lizard_languages lizard_core' in text
        assert 'pylint --exit-zero --rcfile pylintrc' in text
    receipts[name] = receipt
for receipt in proof['receipts'].values():
    assert digest(Path(receipt['stdout'])) == receipt['stdout_sha256']
    assert digest(Path(receipt['stderr'])) == receipt['stderr_sha256']
trace = proof['reused_installed_trace']
assert digest(Path(trace['path'])) == digest(Path(trace['current_copy'])) == trace['sha256']
assert trace['rows'] == 492 and trace['blank_dispositions'] == 0
inputs = json.loads((QUALIFIED / 'augmented-inputs.json').read_text())
author = ast.parse(git('show', proof['merge_head'] + ':test/test_languages/testES6.py'))
methods = [node for node in ast.walk(author) if isinstance(node, ast.FunctionDef)
           and node.name == 'test_colon_of_the_ternary_ends_the_arrow_function']
assert len(methods) == 1
assignment = next(node for node in methods[0].body if isinstance(node, ast.Assign))
authored_source = ast.literal_eval(assignment.value)
assert all(row['source'] == authored_source for row in inputs if row['label'] == 'authored')
workflow = CHECKOUT / '.github/workflows/python-app.yml'
content = workflow.read_text()
oses = ['ubuntu-24.04', 'windows-latest']
versions = ['3.8', '3.9', '3.10', '3.11', '3.12', '3.13', '3.14']
assert all('- ' + name in content for name in oses)
assert all('- "' + version + '"' in content for version in versions)
assert "- '**'" in content and 'needs.changes.outputs.web' in content
source_inputs = ['flake.nix', 'flake.lock', 'requirements.txt', 'dev_requirements.txt',
                 '.github/workflows/python-app.yml', 'scripts/web_changes.py']
assert not git('diff', '--name-only', proof['qualified_revision'], '--', *source_inputs)
files = {}
for name in ('delivery-local-verify.py', 'delivery-local-verification.json',
             'delivery-local-integrity.py', 'delivery-local-proof.md'):
    path = HERE / name
    text = path.read_text()
    assert text.endswith('\n') and not text.endswith('\n\n'), name
    assert all(line == line.rstrip() for line in text.splitlines()), name
    assert len(text.splitlines()) <= 250, name
    if path.suffix == '.py':
        ast.parse(text, filename=str(path))
    if path.suffix == '.json':
        json.loads(text)
    files[name] = dict(sha256=digest(path), lines=len(text.splitlines()))
payload = dict(time=datetime.now(timezone.utc).isoformat(),
               content_identity=dict(head=proof['head'], merge_head=proof['merge_head'],
                   qualified_revision=proof['qualified_revision'], current=proof['current']),
               commands=receipts, verification_command_returncode=0,
               source_inputs_sha256={name: digest(CHECKOUT / name) for name in source_inputs},
               verification_helper_sha256=digest(HERE / 'qualification-verify.py'),
               host_observation_sha256=digest(CACHE / 'host-before.json'),
               exact_authored_source_preserved=True, expected_hosted_matrix=dict(
                   os=oses, python=versions, jobs=14, changes='required', web='conditional; current comparison false'),
               current_hosted_ci='pending coordinator', files=files,
               integrity_returncode=0)
output = HERE / 'delivery-local-checks.json'
output.write_text(json.dumps(payload, separators=(',', ':')) + '\n')
json.loads(output.read_text())
assert len(output.read_text().splitlines()) <= 250
print(json.dumps(dict(files=5, product_hashes=104, test_files=159, authored_source_exact=True,
                      full_pytest=2118, make_pytest=2118, skipped=7, integrity_returncode=0)), flush=True)
