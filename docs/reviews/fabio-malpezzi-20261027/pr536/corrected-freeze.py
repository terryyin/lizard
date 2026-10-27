"""Identify an uncommitted candidate by base, exact patch and product manifest."""
import hashlib
import io
import json
from pathlib import Path
import subprocess
import sys
import tarfile

ROOT = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr536-corrected')
label = sys.argv[1]
base = subprocess.check_output(['git', 'rev-parse', 'HEAD'], text=True).strip()
patch = subprocess.check_output(['git', 'diff', '--binary', base, '--',
                                 'lizard.py', 'lizard_core',
                                 'lizard_languages', 'lizard_ext'])
patch_path = ROOT / (label + '.patch')
assert not patch_path.exists()
patch_path.write_bytes(patch)
snapshot = ROOT / label
snapshot.mkdir()
with tarfile.open(fileobj=io.BytesIO(subprocess.check_output(['git', 'archive', base]))) as stream:
    stream.extractall(snapshot)
if patch:
    subprocess.run(['git', 'apply', '--unsafe-paths', '--directory=' + str(snapshot),
                    str(patch_path)], cwd='/private/tmp', check=True)
manifest = {}
for path in sorted(snapshot.rglob('*')):
    name = str(path.relative_to(snapshot))
    if path.is_file() and (name == 'lizard.py' or name.startswith(
            ('lizard_core/', 'lizard_languages/', 'lizard_ext/'))):
        manifest[name] = hashlib.sha256(path.read_bytes()).hexdigest()
        assert path.read_bytes() == (Path.cwd() / name).read_bytes()
source = (json.dumps(manifest, sort_keys=True) + '\n').encode()
(ROOT / (label + '-product.json')).write_bytes(source)
identity = {'label': label, 'base': base, 'patch_sha256': hashlib.sha256(patch).hexdigest(),
            'product_manifest_sha256': hashlib.sha256(source).hexdigest()}
(ROOT / (label + '-identity.json')).write_text(json.dumps(identity) + '\n')
print(json.dumps(identity))
