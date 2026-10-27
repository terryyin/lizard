"""Freeze the corrected PR534 comparison without changing historical evidence."""

import hashlib
import io
import json
import os
from pathlib import Path
import shutil
import subprocess
import tarfile

HERE = Path(__file__).resolve().parent
CHECKOUT = HERE.parents[3]
HISTORICAL = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534')
DEST = HISTORICAL / 'seed015-performance'
REVISIONS = {
    'candidate': '74bc899db957d27004f9fd58b095e7394fba93dd',
    'current': '9979f106a955aef2460ed6fe7ebb5782b755bcad',
    'frozen': '2e21e404c1ab9f4e0b49acc80816e816629d0670',
}
HARNESS = '3064d8d77047db2f084ce9fd6ae09d98acf34453316b7df20fb184cbb3217bf5'
MANIFEST = '113ca1889836963fe6cfd7ba62c1aa90bd927eaedc4cd8cc7b46479ecb3bccd5'


def digest(data):
    return hashlib.sha256(data).hexdigest()


def command(*args):
    return subprocess.check_output(args, cwd=CHECKOUT, text=True).strip()


def compact_map(path, payload):
    path.write_text('{\n' + ',\n'.join(
        json.dumps(key) + ': ' + json.dumps(value)
        for key, value in payload.items()) + '\n}\n')


assert command('git', 'rev-parse', 'HEAD') == REVISIONS['candidate']
assert not command('git', 'diff', '--name-only')
assert command('git', 'rev-parse', 'origin/master') == REVISIONS['current']
assert digest((HISTORICAL / 'paired.py').read_bytes()) == HARNESS
assert digest((HISTORICAL / 'manifest.json').read_bytes()) == MANIFEST
assert digest((HERE / 'manifest.json').read_bytes()) == MANIFEST
manifest = json.loads((HISTORICAL / 'manifest.json').read_text())
DEST.mkdir(exist_ok=True)
assert not (DEST / 'freeze.json').exists(), 'Do not overwrite an earlier freeze'
corpus = DEST / 'corpus'
corpus.mkdir(exist_ok=True)
for name, expected in manifest['files'].items():
    original = HISTORICAL / 'corpus' / name
    assert digest(original.read_bytes()) == expected, name
    target = corpus / name
    target.parent.mkdir(parents=True, exist_ok=True)
    shutil.copyfile(original, target)
cases = json.loads((HERE / 'inputs.json').read_text())
additions = []
for filename in ('supplemental-inputs.json', 'signature-inputs.json'):
    for case in json.loads((HERE / filename).read_text()):
        assert not (corpus / 'affected' / case['filename']).exists(), case['filename']
        (corpus / 'affected' / case['filename']).write_text(case['source'])
        additions.append(dict(case, addition=filename))
cases += additions
(DEST / 'inputs.json').write_text('[\n' + ',\n'.join(
    json.dumps(case) for case in cases) + '\n]\n')
affected = {str(p.relative_to(corpus)): digest(p.read_bytes())
            for p in sorted((corpus / 'affected').iterdir()) if p.is_file()}
broad = {name: value for name, value in manifest['files'].items()
         if not name.startswith('affected/')}
assert len(affected) == len(cases) == 128
assert len(broad) == 100
compact_map(DEST / 'affected-hashes.json', affected)
compact_map(DEST / 'broad-hashes.json', broad)
compact_map(DEST / 'addition-hashes.json', {
    case['filename']: dict(source_sha256=digest(case['source'].encode()),
                           label=case['addition']) for case in additions})
for name in ('paired.py', 'manifest.json'):
    shutil.copyfile(HISTORICAL / name, DEST / name)
source_hashes = {}
for label, revision in REVISIONS.items():
    snapshot = DEST / label
    snapshot.mkdir(exist_ok=True)
    archive = subprocess.check_output(['git', 'archive', revision], cwd=CHECKOUT)
    with tarfile.open(fileobj=io.BytesIO(archive)) as tree:
        tree.extractall(snapshot)
    files = [snapshot / 'lizard.py']
    for directory in ('lizard_core', 'lizard_languages', 'lizard_ext'):
        files += sorted((snapshot / directory).rglob('*.py'))
    source_hashes[label] = {str(p.relative_to(snapshot)): digest(p.read_bytes())
                            for p in files}
    compact_map(DEST / (label + '-source-hashes.json'), source_hashes[label])
processes = subprocess.check_output(
    ['ps', '-axo', 'pid,pcpu,pmem,command'], text=True).splitlines()
heavy = [line for line in processes if any(term in line for term in (
    'paired.py', 'pytest', 'cProfile', 'profile-control'))]
memory = command('vm_stat')
payload = dict(revisions=REVISIONS, harness_sha256=HARNESS,
               original_manifest_sha256=MANIFEST, affected_files=128,
               broad_files=100, additions=36,
               affected_manifest_sha256=digest((DEST / 'affected-hashes.json').read_bytes()),
               broad_manifest_sha256=digest((DEST / 'broad-hashes.json').read_bytes()),
               addition_manifest_sha256=digest((DEST / 'addition-hashes.json').read_bytes()),
               host=command('hostname'), cpus=os.cpu_count(), load=os.getloadavg(),
               memory_bytes=command('sysctl', '-n', 'hw.memsize'),
               vm_stat=memory, competing_commands=heavy,
               source_hashes={key: digest(json.dumps(value, sort_keys=True).encode())
                              for key, value in source_hashes.items()})
(DEST / 'freeze.json').write_text(json.dumps(payload, indent=2) + '\n')
print(json.dumps(payload))
