"""Freeze PR551 Git change and adapt it onto the current target."""
import hashlib
import io
import json
from pathlib import Path
import shutil
import subprocess
import tarfile

REPO = Path(__file__).resolve().parents[4]
EVIDENCE = Path(__file__).resolve().parent
CACHE = Path('/Users/terryyin/.cache/lizard-pr-reviews-SEED-034')
PRIOR = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027')
ROOT = CACHE / 'pr551'
TARGET = '4a05e4056cebb2edd75cb02b7a43e911369f4ce5'
HEAD = 'a8dabb2351014cbf5164fe31a3472036877130fc'
BASELINE = TARGET  # slice 1: frozen trunk == current target
PRODUCT = [
    'lizard_languages/javascript_objects.py',
    'lizard_languages/typescript_states.py',
    'test/test_languages/testJavaScriptMembers.py',
    'test/test_languages/testTypeScriptFields.py',
]
ROOT.mkdir(parents=True, exist_ok=True)


def archive(label, revision):
    destination = ROOT / label
    if destination.exists():
        shutil.rmtree(destination)
    destination.mkdir()
    data = subprocess.check_output(['git', 'archive', revision], cwd=REPO)
    tarfile.open(fileobj=io.BytesIO(data)).extractall(destination)


for label, revision in [
        ('target', TARGET), ('raw', HEAD), ('parent', HEAD + '^'),
        ('baseline', BASELINE)]:
    archive(label, revision)

raw_patch = subprocess.check_output(['git', 'diff', HEAD + '^', HEAD], cwd=REPO)
corrected = ROOT / 'corrected'
if corrected.exists():
    shutil.rmtree(corrected)
shutil.copytree(ROOT / 'target', corrected)
for name in PRODUCT:
    shutil.copyfile(REPO / name, corrected / name)

adapted_patch = ''.join(
    subprocess.run(
        ['git', 'diff', '--no-index',
         str(ROOT / 'target' / name), str(corrected / name)],
        text=True, stdout=subprocess.PIPE).stdout
    for name in PRODUCT)
patches = {
    'raw': {'revisions': [HEAD + '^', HEAD], 'patch': raw_patch.decode()},
    'adapted': {
        'target': TARGET,
        'source': HEAD,
        'working_tree_product': True,
        'files': PRODUCT,
        'patch': adapted_patch,
    },
}
(EVIDENCE / 'literal-git-patches.json').write_text(json.dumps(patches) + '\n')
for label, record in patches.items():
    normalized = '\n'.join(line.rstrip() for line in record['patch'].splitlines())
    (EVIDENCE / (label + '.diff')).write_text(
        '# DISPLAY ONLY: exact executable bytes in literal-git-patches.json\n'
        + normalized.rstrip() + '\n')

cases = json.loads((EVIDENCE / 'inputs.json').read_text())
folder = ROOT / 'proof'
if folder.exists():
    shutil.rmtree(folder)
(folder / 'corpus/affected').mkdir(parents=True)
# Shared frozen corpus pieces from prior cohort when present.
for name in ('lizard.py', 'lizard_core', 'lizard_languages', 'lizard_ext'):
    source = PRIOR / 'corpus' / name
    destination = folder / 'corpus' / name
    if source.is_dir():
        shutil.copytree(source, destination)
    elif source.exists():
        shutil.copyfile(source, destination)
for case in cases:
    (folder / 'corpus/affected' / case['filename']).write_text(case['source'])

consumers = folder / 'corpus/consumers'
consumers.mkdir(parents=True)
for case in cases:
    (consumers / case['filename']).write_text(case['source'])
# Prior JS-family affected samples from last cohort PR.
for previous in ('pr547', 'pr546', 'pr545', 'pr544', 'pr536', 'pr535'):
    prior = REPO / 'docs/reviews/fabio-malpezzi-20261027' / previous / 'inputs.json'
    if prior.exists():
        for case in json.loads(prior.read_text()):
            path = consumers / case['filename']
            if not path.exists():
                path.write_text(case['source'])
consumer_js = [
    path for path in consumers.iterdir()
    if path.is_file() and path.suffix in {'.js', '.ts', '.jsx', '.tsx'}]
for replica in range(1, 13):
    for path in consumer_js:
        dest = consumers / ('ctrl%s-%s' % (replica, path.name))
        if not dest.exists():
            shutil.copyfile(path, dest)

expanded = list(cases)
for previous in ('pr547', 'pr546', 'pr545', 'pr544', 'pr536', 'pr535'):
    prior = REPO / 'docs/reviews/fabio-malpezzi-20261027' / previous / 'inputs.json'
    if prior.exists():
        for case in json.loads(prior.read_text()):
            if not any(c['filename'] == case['filename'] for c in expanded):
                expanded.append({
                    'filename': case['filename'],
                    'source': case['source'],
                    'expected': case.get('expected'),
                    'control': True,
                })
controls_dir = folder / 'corpus/affected-expanded'
controls_dir.mkdir(parents=True)
for case in expanded:
    (controls_dir / case['filename']).write_text(case['source'])
for prior_name in ('pr547', 'pr546'):
    prior_expanded = PRIOR / prior_name / 'proof' / 'corpus' / 'affected-expanded'
    if not prior_expanded.exists():
        prior_expanded = PRIOR / prior_name / 'proof' / 'corpus' / 'affected'
    if prior_expanded.exists():
        for path in prior_expanded.iterdir():
            if path.is_file() and not (controls_dir / path.name).exists():
                shutil.copyfile(path, controls_dir / path.name)
bulk = PRIOR / 'controls'
if bulk.exists():
    for path in bulk.rglob('*'):
        if path.is_file() and path.suffix in {
                '.js', '.ts', '.jsx', '.tsx', '.vue', '.c', '.cpp'}:
            dest = controls_dir / path.name
            if not dest.exists():
                shutil.copyfile(path, dest)
# Also include the prior broad 100-file corpus reconstruction when present.
broad = PRIOR / 'corpus' / 'affected'
if broad.exists():
    for path in broad.rglob('*'):
        if path.is_file() and path.suffix in {
                '.js', '.ts', '.jsx', '.tsx', '.vue', '.c', '.cpp', '.py'}:
            dest = controls_dir / path.name
            if not dest.exists():
                shutil.copyfile(path, dest)

non_template = [
    path for path in controls_dir.iterdir()
    if path.is_file() and path.suffix in {'.js', '.ts', '.jsx', '.tsx'}
    and '${' not in path.read_text(errors='ignore')]
for replica in range(1, 13):
    for path in non_template:
        dest = controls_dir / ('ctrl%s-%s' % (replica, path.name))
        if not dest.exists():
            shutil.copyfile(path, dest)

corpus_manifest = {
    str(p.relative_to(folder / 'corpus')): hashlib.sha256(p.read_bytes()).hexdigest()
    for p in sorted((folder / 'corpus').rglob('*')) if p.is_file()}
(folder / 'manifest.json').write_text(json.dumps(corpus_manifest) + '\n')
(EVIDENCE / 'manifest.json').write_text(json.dumps(corpus_manifest) + '\n')
(EVIDENCE / 'inputs-expanded.json').write_text(
    json.dumps([
        {'filename': p.name,
         'sha256': hashlib.sha256(p.read_bytes()).hexdigest()}
        for p in sorted(controls_dir.iterdir()) if p.is_file()
    ], indent=2) + '\n')
hashes = {}
for label in ('target', 'raw', 'parent', 'corrected', 'baseline'):
    hashes[label] = {
        str(p.relative_to(ROOT / label)): hashlib.sha256(p.read_bytes()).hexdigest()
        for area in ('lizard_core', 'lizard_languages', 'lizard_ext')
        for p in sorted((ROOT / label / area).rglob('*.py'))}
    hashes[label]['lizard.py'] = hashlib.sha256(
        (ROOT / label / 'lizard.py').read_bytes()).hexdigest()
(ROOT / 'source-hashes.json').write_text(json.dumps(hashes) + '\n')
(EVIDENCE / 'source-hashes.json').write_text(json.dumps(hashes) + '\n')
summary = {
    'target': TARGET,
    'frozen_head': HEAD,
    'baseline': BASELINE,
    'product': PRODUCT,
    'affected_count': len(list((folder / 'corpus/affected').iterdir())),
    'expanded_count': len(list(controls_dir.iterdir())),
    'consumers_count': len(list(consumers.iterdir())),
}
(EVIDENCE / 'bootstrap-summary.json').write_text(json.dumps(summary, indent=2) + '\n')
print(json.dumps(summary, indent=2))
