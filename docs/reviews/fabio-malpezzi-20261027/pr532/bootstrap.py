"""Create immutable source snapshots and rebuild the original broad corpus."""
import hashlib
import io
import json
from pathlib import Path
import shutil
import subprocess
import tarfile

REPO = Path(__file__).resolve().parents[4]
EVIDENCE = REPO / 'docs/reviews/fabio-malpezzi-20261027'
CACHE = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027')
ROOT = CACHE / 'pr532'
ROOT.mkdir(parents=True, exist_ok=True)


def archive(destination, revision):
    if (destination / 'lizard.py').exists():
        return
    destination.mkdir(parents=True, exist_ok=True)
    data = subprocess.check_output(['git', 'archive', revision], cwd=REPO)
    # These are trusted immutable repository archives, not external tar inputs.
    tarfile.open(fileobj=io.BytesIO(data)).extractall(destination)


archive(ROOT / 'target', '33807306953535ebda5c0fa42797b1e5e7ef73d1')
archive(ROOT / 'raw', 'd0713bf5b4dcf4bf24453156a3756688627abbc0')
archive(CACHE / 'baseline', '2e21e404c1ab9f4e0b49acc80816e816629d0670')
if not (ROOT / 'raw-target').exists():
    shutil.copytree(ROOT / 'target', ROOT / 'raw-target')
    shutil.copyfile(ROOT / 'raw/lizard_languages/javascript_functions.py',
                    ROOT / 'raw-target/lizard_languages/javascript_functions.py')
manifest = json.loads((EVIDENCE / 'manifest.json').read_text())
for name, expected in manifest['files'].items():
    if name.startswith('affected/'):
        continue
    data = subprocess.check_output(['git', 'show', manifest['revision'] + ':' + name],
                                   cwd=REPO)
    assert hashlib.sha256(data).hexdigest() == expected, name
    target = CACHE / 'corpus' / name
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_bytes(data)
print(json.dumps({'root': str(ROOT), 'source_snapshots': ['target', 'raw', 'raw-target'],
                  'starting_baseline': str(CACHE / 'baseline')}))
