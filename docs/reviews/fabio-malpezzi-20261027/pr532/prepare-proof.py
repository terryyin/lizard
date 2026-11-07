"""Freeze candidate code, source inputs and the shared paired harness."""
import hashlib
import json
from pathlib import Path
import shutil
import subprocess
import sys

REPO = Path(__file__).resolve().parents[4]
CACHE = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027')
ROOT = CACHE / 'pr532'
PRODUCT = ['lizard_core/function_info.py',
           'lizard_languages/javascript_bindings.py',
           'lizard_languages/javascript_functions.py',
           'lizard_languages/typescript.py', 'lizard_languages/tsx.py']
corrected = ROOT / 'corrected'
if not corrected.exists():
    shutil.copytree(ROOT / 'target', corrected)
for name in PRODUCT + ['test/test_languages/testJavaScriptParameterNames.py']:
    shutil.copyfile(REPO / name, corrected / name)
inputs = json.loads((REPO / 'docs/reviews/fabio-malpezzi-20261027/pr532/inputs.json').read_text())
manifest = {}
for mode in ('affected', 'consumers'):
    folder = ROOT / mode
    (folder / 'corpus/affected').mkdir(parents=True, exist_ok=True)
    shutil.copyfile(REPO / 'docs/reviews/fabio-malpezzi-20261027/paired.py',
                    folder / 'paired.py')
    assert hashlib.sha256((folder / 'paired.py').read_bytes()).hexdigest() == (
        'ec1c9c5eac0ad6a29294ecbd987b2a55d4cf31db145486d065bf13c096b76520')
    for name in ('lizard.py', 'lizard_core', 'lizard_languages', 'lizard_ext'):
        path = folder / 'corpus' / name
        if not path.exists():
            if (CACHE / 'corpus' / name).is_dir():
                shutil.copytree(CACHE / 'corpus' / name, path)
            else:
                shutil.copyfile(CACHE / 'corpus' / name, path)
    selected = [case for case in inputs
                if mode == 'consumers' or case['filename'].split('.')[-1]
                in ('js', 'ts', 'jsx', 'tsx', 'vue')]
    for case in selected:
        path = folder / 'corpus/affected' / case['filename']
        path.write_text(case['source'])
    manifest[mode] = {
        str(path.relative_to(folder / 'corpus')):
        hashlib.sha256(path.read_bytes()).hexdigest()
        for path in sorted((folder / 'corpus').rglob('*')) if path.is_file()}
    (folder / 'manifest.json').write_text(json.dumps(manifest[mode], indent=2) + '\n')
snapshots = {}
for name in ('target', 'raw-target', 'corrected'):
    snapshots[name] = {path: hashlib.sha256((ROOT / name / path).read_bytes()).hexdigest()
                       for path in PRODUCT}
(ROOT / 'snapshot-hashes.json').write_text(json.dumps(snapshots, indent=2) + '\n')
print(json.dumps({'inputs': len(inputs), 'snapshots': snapshots,
                  'harness_sha256': hashlib.sha256((ROOT / 'affected/paired.py').read_bytes()).hexdigest()}))
