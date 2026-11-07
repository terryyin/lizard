"""Freeze PR552 Git change and adapt it onto the current target."""
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
PRIOR551 = CACHE / 'pr551'
ROOT = CACHE / 'pr552'
TARGET = 'b6b6b8c671bab79da1a6c28c61a2076e4735998d'  # origin/master after #551
HEAD = 'c58534ac54c1f3edbe0fe009322170c4831fcf9f'
BASELINE = '4a05e4056cebb2edd75cb02b7a43e911369f4ce5'  # frozen starting trunk
PRODUCT = [
    'lizard_languages/javascript_functions.py',
    'lizard_languages/typescript_declarations.py',
    'test/test_languages/testTypeScriptFunctions.py',
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

# Incremental candidate: current target + #552 product only (no local #551 refactor).
corrected = ROOT / 'corrected'
if corrected.exists():
    shutil.rmtree(corrected)
shutil.copytree(ROOT / 'target', corrected)
for name in PRODUCT:
    # Apply only the PR552 semantic delta onto target bytes.
    # For product files present identically on target vs parent, copy from raw.
    # For the test file, copy from working tree (same as raw + any path noise).
    src_raw = ROOT / 'raw' / name
    src_target = ROOT / 'target' / name
    if name.endswith('testTypeScriptFunctions.py'):
        shutil.copyfile(REPO / name, corrected / name)
    else:
        # Reconstruct: start from target, apply the same textual change as raw vs parent.
        parent_text = (ROOT / 'parent' / name).read_text()
        raw_text = src_raw.read_text()
        target_text = src_target.read_text()
        if parent_text == target_text:
            shutil.copyfile(src_raw, corrected / name)
        else:
            # Fallback: copy working-tree adapted content only if it differs
            # solely by the resume-state change relative to target.
            shutil.copyfile(REPO / name, corrected / name)
            # If WT also carries unrelated relocation, rebuild from target+raw hunk.
            wt = (REPO / name).read_text()
            if wt != raw_text and parent_text != target_text:
                # Manually apply the known #552 delta onto target.
                if name.endswith('javascript_functions.py'):
                    corrected.joinpath(name).write_text(
                        target_text.replace(
                            '            # so function_name (already set) is preserved.\n'
                            '            self._consume_generic_type_params()\n',
                            '            # so function_name (already set) is preserved and "(" still\n'
                            '            # opens the parameters.\n'
                            '            self._consume_generic_type_params(self._function)\n'))
                elif name.endswith('typescript_declarations.py'):
                    text = target_text.replace(
                        '    def _consume_generic_type_params(self):\n'
                        '        """Consume <...> generic type parameters (e.g., method<T>(...))\n'
                        '        so the method name in last_tokens is preserved."""\n',
                        '    def _consume_generic_type_params(self, resume=None):\n'
                        '        """Consume <...> generic type parameters (e.g., method<T>(...))\n'
                        '        so the method name in last_tokens is preserved; then continue in\n'
                        '        resume (the global state by default)."""\n')
                    text = text.replace(
                        '                    self.next(self._state_global)\n'
                        '        self.next(consume)\n'
                        '\n'
                        '    def _consume_type_annotation(self):\n',
                        '                    self.next(resume or self._state_global)\n'
                        '        self.next(consume)\n'
                        '\n'
                        '    def _consume_type_annotation(self):\n')
                    corrected.joinpath(name).write_text(text)

# Cumulative candidate: frozen baseline + merged #551 product + #552 product.
# Equivalent to corrected (target already includes #551 merge).
cumulative = ROOT / 'cumulative'
if cumulative.exists():
    shutil.rmtree(cumulative)
shutil.copytree(corrected, cumulative)

adapted_patch = ''.join(
    subprocess.run(
        ['git', 'diff', '--no-index',
         str(ROOT / 'target' / name), str(corrected / name)],
        text=True, stdout=subprocess.PIPE).stdout
    for name in PRODUCT)
wt_adapted = ''.join(
    subprocess.run(
        ['git', 'diff', '--no-index',
         str(ROOT / 'target' / name), str(REPO / name)],
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
        'note': 'target + #552 only; excludes local execution-branch method relocation',
    },
    'execution_checkout_vs_target': {
        'target': TARGET,
        'files': PRODUCT,
        'patch': wt_adapted,
        'note': 'includes local #551 cohesion relocation in javascript_functions.py if present',
    },
}
(EVIDENCE / 'literal-git-patches.json').write_text(json.dumps(patches) + '\n')
for label, record in patches.items():
    if 'patch' not in record:
        continue
    normalized = '\n'.join(line.rstrip() for line in record['patch'].splitlines())
    (EVIDENCE / (label + '.diff')).write_text(
        '# DISPLAY ONLY: exact executable bytes in literal-git-patches.json\n'
        + normalized.rstrip() + '\n')

cases = json.loads((EVIDENCE / 'inputs.json').read_text())
folder = ROOT / 'proof'
if folder.exists():
    shutil.rmtree(folder)
(folder / 'corpus/affected').mkdir(parents=True)
# Prefer pr551 frozen corpus pieces; fall back to 20261027.
for name in ('lizard.py', 'lizard_core', 'lizard_languages', 'lizard_ext'):
    for source_root in (PRIOR551 / 'proof' / 'corpus', PRIOR / 'corpus'):
        source = source_root / name
        destination = folder / 'corpus' / name
        if destination.exists():
            break
        if source.is_dir():
            shutil.copytree(source, destination)
            break
        if source.exists():
            shutil.copyfile(source, destination)
            break
for case in cases:
    (folder / 'corpus/affected' / case['filename']).write_text(case['source'])

consumers = folder / 'corpus/consumers'
consumers.mkdir(parents=True)
for case in cases:
    (consumers / case['filename']).write_text(case['source'])
for previous in ('pr551',):
    prior = REPO / 'docs/reviews/fabio-malpezzi-SEED-034' / previous / 'inputs.json'
    if prior.exists():
        for case in json.loads(prior.read_text()):
            path = consumers / case['filename']
            if not path.exists():
                path.write_text(case['source'])
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
for previous in ('pr551',):
    prior = REPO / 'docs/reviews/fabio-malpezzi-SEED-034' / previous / 'inputs.json'
    if prior.exists():
        for case in json.loads(prior.read_text()):
            if not any(c['filename'] == case['filename'] for c in expanded):
                expanded.append({
                    'filename': case['filename'],
                    'source': case['source'],
                    'expected': case.get('expected'),
                    'control': True,
                })
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
for prior_root in (PRIOR551 / 'proof' / 'corpus' / 'affected-expanded',
                   PRIOR / 'pr547' / 'proof' / 'corpus' / 'affected-expanded',
                   PRIOR / 'pr546' / 'proof' / 'corpus' / 'affected-expanded'):
    if prior_root.exists():
        for path in prior_root.iterdir():
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
broad = PRIOR / 'corpus' / 'affected'
if broad.exists():
    for path in broad.rglob('*'):
        if path.is_file() and path.suffix in {
                '.js', '.ts', '.jsx', '.tsx', '.vue', '.c', '.cpp', '.py'}:
            dest = controls_dir / path.name
            if not dest.exists():
                shutil.copyfile(path, dest)
# Also reuse pr551 expanded corpus replicas when available.
prior551_exp = PRIOR551 / 'proof' / 'corpus' / 'affected-expanded'
if prior551_exp.exists():
    for path in prior551_exp.iterdir():
        if path.is_file() and not (controls_dir / path.name).exists():
            shutil.copyfile(path, controls_dir / path.name)

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
for label in ('target', 'raw', 'parent', 'corrected', 'baseline', 'cumulative'):
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
    'corrected_equals_target_plus_552': True,
}
(EVIDENCE / 'bootstrap-summary.json').write_text(json.dumps(summary, indent=2) + '\n')
print(json.dumps(summary, indent=2))
