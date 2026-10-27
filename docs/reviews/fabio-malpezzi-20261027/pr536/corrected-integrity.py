"""Check retained evidence bytes, escaped raw chunks, EOF and source identity."""
import hashlib
import json
from pathlib import Path

HERE = Path(__file__).resolve().parent
PROOF = HERE / 'corrected-proof'
CHECKOUT = HERE.parents[3]


def sha(data):
    return hashlib.sha256(data).hexdigest()


manifest = json.loads((PROOF / 'evidence-manifest.json').read_text())
assert set(manifest) == {p.name for p in PROOF.iterdir()} - {'evidence-manifest.json'}
chunks = {}
for name, digest in manifest.items():
    path = PROOF / name
    assert sha(path.read_bytes()) == digest
    if '.part' in name and path.suffix == '.json':
        part = json.loads(path.read_text())
        assert sha(part['text'].encode()) == part['chunk_sha256']
        chunks.setdefault(part['source'], []).append(part)
for parts in chunks.values():
    data = ''.join(part['text'] for part in sorted(parts, key=lambda p: p['start_line'])).encode()
    assert sha(data) == parts[0]['source_sha256']
product = json.loads((PROOF / 'candidate-5-product.json').read_text())
for name, digest in product.items():
    assert sha((CHECKOUT / name).read_bytes()) == digest
records = list(PROOF.iterdir()) + list(HERE.glob('corrected-*')) + [HERE / 'slice4-proof.md']
for path in records:
    if not path.is_file():
        continue
    data = path.read_bytes()
    assert data.endswith(b'\n'), path
    assert len(data.splitlines()) <= 250, path
    assert not any(line.endswith((b' ', b'\t')) for line in data.splitlines()), path
print('PASS: manifests, exact raw chunks, measured product bytes, <=250 lines, whitespace and EOF.')
