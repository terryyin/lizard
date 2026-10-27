"""Retain bounded logs and verify immutable inputs and whitespace explicitly."""
import difflib
import hashlib
import json
from pathlib import Path

HERE = Path(__file__).resolve().parent
CACHE = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534')
manifest_bytes = (HERE / 'manifest.json').read_bytes()
manifest = json.loads(manifest_bytes)
for name, digest in manifest['files'].items():
    assert hashlib.sha256((CACHE / 'corpus' / name).read_bytes()).hexdigest() == digest, name
for workload in ('api', 'cli'):
    result = json.loads((CACHE / ('534-' + workload + '.json')).read_text())
    assert result['manifest_sha256'] == hashlib.sha256(manifest_bytes).hexdigest()
    assert all(min(row.values()) >= 1 for row in result['raw']), result
for path in sorted(CACHE.glob('*')):
    if not path.is_file() or path.suffix not in ('.log', '.json'):
        continue
    if path.name.endswith('-broad-cli.log') or path.name in ('inputs.json', 'manifest.json'):
        continue
    value = path.read_text()
    assert len(value.splitlines()) <= 250, (path, len(value.splitlines()))
    if path.suffix == '.log':
        value = '\n'.join(line.rstrip() for line in value.splitlines()) + '\n'
    (HERE / path.name).write_text(value)
inputs = {case['filename']: case for case in json.loads((HERE / 'inputs.json').read_text())}
for name in ('name', 'curried'):
    records = (CACHE / ('raw-expected-' + name + '-diagnostic.log')).read_text().splitlines()[1:-1]
    reconstructed = [dict(inputs[row['case']], expected=row['expected'])
                     for row in map(json.loads, records)]
    (HERE / ('initial-' + name + '-inputs.json')).write_text(
        '[\n' + ',\n'.join(json.dumps(case) for case in reconstructed) + '\n]\n')
patch = ''
hashes = {}
for name in ('lizard_languages/javascript_functions.py', 'lizard_languages/typescript_states.py'):
    target = (CACHE / 'target' / name).read_text()
    child = (CACHE / 'child' / name).read_text()
    patch += ''.join(difflib.unified_diff(target.splitlines(True), child.splitlines(True),
                                         fromfile='a/' + name, tofile='b/' + name))
    hashes[name] = {snapshot: hashlib.sha256((CACHE / snapshot / name).read_bytes()).hexdigest()
                    for snapshot in ('target', 'raw', 'child')}
(HERE / 'independent-child-patch.json').write_text(json.dumps(dict(
    basis='7186fdbe826bf32b399165cc3d36897a128d4d0d', patch=patch)) + '\n')
original = HERE / 'proposed.patch'
if original.exists():
    data = original.read_bytes()
    (CACHE / 'proposed.patch').write_bytes(data)
    (HERE / 'proposed-patch.json').write_text(json.dumps(dict(
        base='01afe9c228ba2a3dc5253259d8415d2ef47f0b71',
        head='f4074ae860c7999cc37dd081a6f7ebfa777adc83', patch=data.decode())) + '\n')
    original.unlink()
(HERE / 'snapshot-hashes.json').write_text(json.dumps(hashes, indent=2) + '\n')
for path in sorted(HERE.iterdir()):
    if not path.is_file():
        continue
    data = path.read_text()
    assert len(data.splitlines()) <= 250, (path, len(data.splitlines()))
    assert all(line == line.rstrip() for line in data.splitlines()), path
summaries = {}
for snapshot in ('target', 'raw', 'child', 'baseline'):
    rows = (HERE / (snapshot + '-reproduction.log')).read_text().splitlines()
    summary = json.loads(rows[-1])
    summaries[snapshot] = dict(cases=summary['cases'], failures=len(summary['failures']))
print(json.dumps(dict(corpus_files=len(manifest['files']), cases=len(inputs),
                      summaries=summaries, immutable_hashes=True,
                      files_below_251_lines=True, trailing_whitespace=False)))
