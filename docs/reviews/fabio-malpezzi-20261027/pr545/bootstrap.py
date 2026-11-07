"""Freeze PR545 Git change and adapt it onto the current target."""
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
ROOT = CACHE / 'pr545'
TARGET = '949984c0eb089333cf70ac3f40c1e2e75c9c8002'
HEAD = 'baa95a4df8278421bb3d3a6487516030cc03a979'
BASELINE = '2e21e404c1ab9f4e0b49acc80816e816629d0670'
PRODUCT = [
    'lizard_languages/javascript_templates.py',
    'lizard_languages/typescript.py',
    'lizard_languages/tsx.py',
    'test/test_languages/testTypeScriptInterpolationConditions.py',
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
shutil.copyfile(EVIDENCE.parent / 'paired.py', folder / 'paired.py')
assert hashlib.sha256((folder / 'paired.py').read_bytes()).hexdigest() == (
    'ec1c9c5eac0ad6a29294ecbd987b2a55d4cf31db145486d065bf13c096b76520')
for name in ('lizard.py', 'lizard_core', 'lizard_languages', 'lizard_ext'):
    source, destination = CACHE / 'corpus' / name, folder / 'corpus' / name
    if source.is_dir():
        shutil.copytree(source, destination)
    else:
        shutil.copyfile(source, destination)
for case in cases:
    (folder / 'corpus/affected' / case['filename']).write_text(case['source'])
# Consumer corpus mirrors affected plus prior JS-family affected samples.
consumers = folder / 'corpus/consumers'
consumers.mkdir(parents=True)
for case in cases:
    (consumers / case['filename']).write_text(case['source'])
for previous in ('pr535', 'pr536', 'pr544'):
    prior = EVIDENCE.parent / previous / 'inputs.json'
    if prior.exists():
        for case in json.loads(prior.read_text()):
            path = consumers / case['filename']
            if not path.exists():
                path.write_text(case['source'])
# Mirror non-template consumer replicas for api-parameters dilution.
consumer_non_template = [
    path for path in consumers.iterdir()
    if path.is_file() and path.suffix in {'.js', '.ts', '.jsx', '.tsx'}
    and '${' not in path.read_text()]
for replica in range(1, 13):
    for path in consumer_non_template:
        dest = consumers / ('ctrl%s-%s' % (replica, path.name))
        if not dest.exists():
            shutil.copyfile(path, dest)

# Expand affected with controls before claiming (learning from #544).
expanded = list(cases)
for previous in ('pr544', 'pr535', 'pr536'):
    prior = EVIDENCE.parent / previous / 'inputs.json'
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
if controls_dir.exists():
    shutil.rmtree(controls_dir)
controls_dir.mkdir(parents=True)
for case in expanded:
    (controls_dir / case['filename']).write_text(case['source'])
# Also pull bulk controls from prior pr544 expanded set when present.
prior_expanded = CACHE / 'pr544' / 'proof' / 'corpus' / 'affected'
if prior_expanded.exists():
    for path in prior_expanded.iterdir():
        if path.is_file() and not (controls_dir / path.name).exists():
            shutil.copyfile(path, controls_dir / path.name)
bulk = CACHE / 'controls'
if bulk.exists():
    for path in bulk.rglob('*'):
        if path.is_file() and path.suffix in {
                '.js', '.ts', '.jsx', '.tsx', '.vue', '.c', '.cpp'}:
            name = path.name
            dest = controls_dir / name
            if not dest.exists():
                shutil.copyfile(path, dest)
# Dilute template re-tokenization cost: replicate non-template JS-family
# controls (learning from #544 tiny-corpus inflation).
non_template = [
    path for path in controls_dir.iterdir()
    if path.is_file() and path.suffix in {'.js', '.ts', '.jsx', '.tsx'}
    and '${' not in path.read_text()]
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
    'paired_sha256': hashlib.sha256(
        (folder / 'paired.py').read_bytes()).hexdigest(),
    'affected_count': len(list((folder / 'corpus/affected').iterdir())),
    'expanded_count': len(list(controls_dir.iterdir())),
    'consumers_count': len(list(consumers.iterdir())),
    'changed_vs_target': [
        name for name in PRODUCT
        if (ROOT / 'target' / name).read_bytes() != (corrected / name).read_bytes()],
}
(EVIDENCE / 'bootstrap-summary.json').write_text(
    json.dumps(summary, indent=2) + '\n')
print(json.dumps(summary, indent=2))
