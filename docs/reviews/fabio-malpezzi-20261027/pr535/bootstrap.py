"""Freeze PR535 provenance, current target, and the accepted shared corpus."""
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
ROOT = CACHE / 'pr535'
ROOT.mkdir(parents=True, exist_ok=True)
TARGET = '3a11bc6a5452be40a32721b5f6e2ab2fa787ca83'
HEAD = '171079e625966ac9c17aee29a5d74256befd85ae'


def archive(name, revision):
    destination = ROOT / name
    if destination.exists():
        return
    destination.mkdir()
    data = subprocess.check_output(['git', 'archive', revision], cwd=REPO)
    # Immutable trusted repository objects; compatible with Nix Python3.9.
    tarfile.open(fileobj=io.BytesIO(data)).extractall(destination)


archive('target', TARGET)
archive('raw', HEAD)
archive('parent', HEAD + '^')
child = ROOT / 'raw-child'
if not child.exists():
    shutil.copytree(ROOT / 'target', child)
    path = child / 'lizard_languages/javascript_functions.py'
    code = path.read_text()
    code = code.replace('        event = self._read_parameter_binding(token)',
        "        first_token = self._parameter_bindings.previous in ('', ',')\n"
        '        event = self._read_parameter_binding(token)', 1)
    code = code.replace("            elif token == ':' and self._parameter_bindings.at_root:",
        "            elif (token == ':' and self._parameter_bindings.at_root) or (\n"
        "                    token == 'this' and first_token):", 1)
    path.write_text(code)

cases = json.loads((EVIDENCE / 'inputs.json').read_text())
cases += json.loads((EVIDENCE.parent / 'pr532/inputs.json').read_text())
(ROOT / 'all-inputs.json').write_text(json.dumps(cases, indent=2) + '\n')
for mode in ('affected', 'consumers'):
    folder = ROOT / mode
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
        if mode == 'consumers' or case['filename'].split('.')[-1] in (
                'js', 'ts', 'jsx', 'tsx', 'vue'):
            (folder / 'corpus/affected' / case['filename']).write_text(case['source'])
    manifest = {str(path.relative_to(folder / 'corpus')):
                hashlib.sha256(path.read_bytes()).hexdigest()
                for path in sorted((folder / 'corpus').rglob('*')) if path.is_file()}
    (folder / 'manifest.json').write_text(json.dumps(manifest, indent=2) + '\n')
    shutil.copyfile(folder / 'manifest.json', EVIDENCE / (mode + '-manifest.json'))
patches = {}
for label, revisions in [('full', ['01afe9c', HEAD]), ('child', [HEAD + '^', HEAD])]:
    patch = subprocess.check_output(['git', 'diff'] + revisions, cwd=REPO, text=True)
    patches[label] = {'revisions': revisions, 'patch': patch}
    normalized = '\n'.join(line.rstrip() for line in patch.splitlines()).rstrip() + '\n'
    (EVIDENCE / (label + '.diff')).write_text(
        '# DISPLAY ONLY: normalized whitespace; exact bytes in literal-git-patches.json\n' + normalized)
(EVIDENCE / 'literal-git-patches.json').write_text(json.dumps(patches) + '\n')
product = ['lizard.py'] + [str(p.relative_to(ROOT / 'target'))
    for area in ('lizard_core', 'lizard_languages', 'lizard_ext')
    for p in sorted((ROOT / 'target' / area).rglob('*.py'))]
hashes = {label: {name: hashlib.sha256((ROOT / label / name).read_bytes()).hexdigest()
                 for name in product} for label in ('target', 'raw', 'raw-child')}
(ROOT / 'initial-hashes.json').write_text(json.dumps(hashes, indent=2) + '\n')
print(json.dumps({'target': TARGET, 'raw': HEAD, 'cases': len(cases),
                  'product_files': len(product), 'root': str(ROOT)}))
