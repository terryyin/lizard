"""Freeze PR544 Git change and adapt it onto the current target."""
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
ROOT = CACHE / 'pr544'
TARGET = 'aa5f40638b23aa54d4724c9429c1ead2183013fd'
HEAD = '3ac7e4eaf61e898836fded88ab715b37f9fedbbf'
BASELINE = '2e21e404c1ab9f4e0b49acc80816e816629d0670'
PRODUCT = [
    'lizard_languages/javascript_templates.py',
    'lizard_languages/typescript.py',
    'lizard_languages/tsx.py',
    'test/test_languages/testJavaScript.py',
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
for previous in ('pr535', 'pr536'):
    prior = EVIDENCE.parent / previous / 'inputs.json'
    if prior.exists():
        for case in json.loads(prior.read_text()):
            path = consumers / case['filename']
            if not path.exists():
                path.write_text(case['source'])

manifest = {
    str(p.relative_to(folder / 'corpus')): hashlib.sha256(p.read_bytes()).hexdigest()
    for p in sorted((folder / 'corpus').rglob('*')) if p.is_file()}
(folder / 'manifest.json').write_text(json.dumps(manifest) + '\n')
(EVIDENCE / 'manifest.json').write_text(json.dumps(manifest) + '\n')

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

workflow = ROOT / 'target' / '.github' / 'workflows' / 'python-app.yml'
if workflow.exists():
    (EVIDENCE / 'required-workflow.yml').write_text(workflow.read_text().rstrip() + '\n')

print(json.dumps({
    'target': TARGET,
    'head': HEAD,
    'baseline': BASELINE,
    'cases': len(cases),
    'files': len(manifest),
    'source_files': len(hashes['target']),
    'changed_vs_target': [
        name for name in PRODUCT
        if (ROOT / 'target' / name).read_bytes() != (corrected / name).read_bytes()],
}))
