"""Verify immutable historical harness retrieval and literal replay consumers."""
import ast
import hashlib
import json
from pathlib import Path
import subprocess
import sys

REPO = Path(__file__).resolve().parents[4]
EVIDENCE = REPO / 'docs/reviews/fabio-malpezzi-20261027'
ROOT = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr532')
old_hash = '3064d8d77047db2f084ce9fd6ae09d98acf34453316b7df20fb184cbb3217bf5'
old = subprocess.check_output(['git', 'show',
    'af27c910a6a0bff51d8358fde0c37fa9cb059d1a:docs/reviews/fabio-malpezzi-20261027/paired.py'], cwd=REPO)
assert hashlib.sha256(old).hexdigest() == old_hash
dest = ROOT / 'historical-freeze-replay'
dest.mkdir(exist_ok=True)
source = (EVIDENCE / 'pr534/freeze.py').read_text()
tree = ast.parse(source)
start = next(i for i, statement in enumerate(tree.body)
             if isinstance(statement, ast.Assign) and any(
                 isinstance(target, ast.Name) and target.id == 'historical_harness'
                 for target in statement.targets))
statements = tree.body[start:start + 3]
namespace = dict(HERE=EVIDENCE / 'pr534', DEST=dest,
                 subprocess=subprocess, hashlib=hashlib)
exec(compile(ast.Module(body=statements, type_ignores=[]),
             str(EVIDENCE / 'pr534/freeze.py'), 'exec'), namespace)
assert (dest / 'paired.py').read_bytes() == old
results = {}
for relative in ('527-cli-ns.json', '527-python-ns.json',
                 'controls/527-python-controls.json', 'pr528/528-api.json',
                 'pr528/528-cli.json', 'pr531/531-api.json', 'pr531/531-cli.json',
                 'pr533/533-api.json', 'pr533/533-cli.json',
                 'pr534/534-api.json', 'pr534/534-cli.json'):
    recorded = json.loads((EVIDENCE / relative).read_text())
    path = Path(recorded['command'][0])
    assert path.read_bytes() == old, path
    results[relative] = dict(path=str(path), sha256=old_hash)
help_result = subprocess.run([sys.executable, str(dest / 'paired.py'), '--help'],
                             cwd='/private/tmp', stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE)
assert help_result.returncode == 0, help_result.stderr
result = dict(recorded_consumers=results, historical_freeze_fragment='\n'.join(
    ast.get_source_segment(source, statement) for statement in statements),
    recovered_hash=hashlib.sha256((dest / 'paired.py').read_bytes()).hexdigest(),
    help_exit=help_result.returncode,
    current_harness_hash=hashlib.sha256((EVIDENCE / 'paired.py').read_bytes()).hexdigest())
(ROOT / 'harness-replay.json').write_text(json.dumps(result, indent=2) + '\n')
print(json.dumps({'historical_consumers': len(results), 'hash': old_hash,
                  'actual_freeze_fragment': True, 'help_exit': help_result.returncode}))
