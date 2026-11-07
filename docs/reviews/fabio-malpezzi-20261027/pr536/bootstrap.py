"""Freeze PR536's exact Git change and apply it to the corrected target."""
import hashlib
import io
import json
from pathlib import Path
import shutil
import subprocess
import tarfile

REPO = Path(__file__).resolve().parents[4]
EVIDENCE = Path(__file__).resolve().parent
CACHE = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027')
ROOT = CACHE / 'pr536'
TARGET = 'd7be8b90ac5872faf86e329e7d7249c98029f045'
HEAD = '92fff8a1621ab82b1ce296d25ac4eb2cdd892c8f'
ROOT.mkdir(parents=True, exist_ok=True)


def archive(label, revision):
    destination = ROOT / label
    if not destination.exists():
        destination.mkdir()
        data = subprocess.check_output(['git', 'archive', revision], cwd=REPO)
        tarfile.open(fileobj=io.BytesIO(data)).extractall(destination)


for label, revision in [('target', TARGET), ('raw', HEAD), ('parent', HEAD + '^')]:
    archive(label, revision)
patch = subprocess.check_output(['git', 'diff', HEAD + '^', HEAD], cwd=REPO)
patches = {'raw': {'revisions': [HEAD + '^', HEAD], 'patch': patch.decode()}}
adapted = ROOT / 'adapted'
if not adapted.exists():
    shutil.copytree(ROOT / 'target', adapted)
    subprocess.run(['git', 'apply', '--check'], input=patch, cwd=adapted, check=True)
    subprocess.run(['git', 'apply'], input=patch, cwd=adapted, check=True)
files = ['lizard_languages/javascript_bindings.py',
         'lizard_languages/typescript_states.py', 'test/test_languages/testES6.py']
patches['adapted'] = {'target': TARGET, 'source': HEAD, 'files': files,
    'patch': ''.join(subprocess.run(['git', 'diff', '--no-index',
        str(ROOT / 'target' / name), str(adapted / name)], text=True,
        stdout=subprocess.PIPE).stdout for name in files)}
(EVIDENCE / 'literal-git-patches.json').write_text(json.dumps(patches) + '\n')
for label, record in patches.items():
    normalized = '\n'.join(line.rstrip() for line in record['patch'].splitlines())
    (EVIDENCE / (label + '.diff')).write_text(
        '# DISPLAY ONLY: exact executable bytes in literal-git-patches.json\n'
        + normalized.rstrip() + '\n')
cases = json.loads((EVIDENCE / 'inputs.json').read_text())
for previous in ('pr532', 'pr535'):
    cases += json.loads((EVIDENCE.parent / previous / 'inputs.json').read_text())
(ROOT / 'all-inputs.json').write_text(json.dumps(cases) + '\n')
folder = ROOT / 'proof'
(folder / 'corpus/affected').mkdir(parents=True, exist_ok=True)
shutil.copyfile(EVIDENCE.parent / 'paired.py', folder / 'paired.py')
assert hashlib.sha256((folder / 'paired.py').read_bytes()).hexdigest() == (
    'ec1c9c5eac0ad6a29294ecbd987b2a55d4cf31db145486d065bf13c096b76520')
for name in ('lizard.py', 'lizard_core', 'lizard_languages', 'lizard_ext'):
    source, destination = CACHE / 'corpus' / name, folder / 'corpus' / name
    if not destination.exists():
        if source.is_dir():
            shutil.copytree(source, destination)
        else:
            shutil.copyfile(source, destination)
for case in cases:
    (folder / 'corpus/affected' / case['filename']).write_text(case['source'])
manifest = {str(p.relative_to(folder / 'corpus')):
    hashlib.sha256(p.read_bytes()).hexdigest()
    for p in sorted((folder / 'corpus').rglob('*')) if p.is_file()}
(folder / 'manifest.json').write_text(json.dumps(manifest) + '\n')
(EVIDENCE / 'manifest.json').write_text(json.dumps(manifest) + '\n')
hashes = {label: {str(p.relative_to(ROOT / label)):
    hashlib.sha256(p.read_bytes()).hexdigest()
    for area in ('lizard_core', 'lizard_languages', 'lizard_ext')
    for p in sorted((ROOT / label / area).rglob('*.py'))}
    for label in ('target', 'raw', 'parent', 'adapted')}
for label in hashes:
    hashes[label]['lizard.py'] = hashlib.sha256(
        (ROOT / label / 'lizard.py').read_bytes()).hexdigest()
(ROOT / 'source-hashes.json').write_text(json.dumps(hashes) + '\n')
for suffix, command in [
    ('metadata.json', ['gh', 'pr', 'view', '536', '--repo', 'terryyin/lizard',
        '--json', 'number,title,body,state,headRefOid,baseRefOid,comments,reviews,statusCheckRollup']),
    ('inline-comments.json', ['gh', 'api', 'repos/terryyin/lizard/pulls/536/comments'])]:
    result = subprocess.check_output(command, cwd=REPO, text=True)
    (ROOT / suffix).write_text(result.rstrip() + '\n')
(EVIDENCE / 'required-workflow.yml').write_text(
    (ROOT / 'target/.github/workflows/python-app.yml').read_text().rstrip() + '\n')
print(json.dumps({'target': TARGET, 'head': HEAD, 'cases': len(cases),
                  'files': len(manifest), 'source_files': len(hashes['target'])}))
