"""Run isolated public regression and unchanged-default observations."""
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys

REPO = Path(__file__).resolve().parents[4]
EVIDENCE = REPO / 'docs/reviews/fabio-malpezzi-20261027/pr532'
CACHE = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027')
ROOT = CACHE / 'pr532'
for name in ('target', 'raw-target', 'corrected', 'baseline'):
    checkout = CACHE / 'baseline' if name == 'baseline' else ROOT / name
    result = subprocess.run([sys.executable, str(EVIDENCE / 'reproduce.py'),
                             str(checkout), str(EVIDENCE / 'inputs.json')],
                            cwd='/private/tmp', text=True,
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    (ROOT / (name + '-final-reproduction.log')).write_text(result.stdout)
    print(name, result.returncode, result.stdout.splitlines()[-1])

suite = ['-q', 'test/test_languages', 'test/test_extensions',
         'test/test_analyzer.py', 'test/testOutput.py',
         'test/testApplication.py', 'test/test_options.py']
for name in ('raw-target', 'corrected'):
    checkout = ROOT / name
    if name == 'raw-target':
        for filename in ('testJavaScriptFunctions.py', 'testTypeScriptMetrics.py'):
            shutil.copyfile(ROOT / 'raw/test/test_languages' / filename,
                            checkout / 'test/test_languages' / filename)
    result = subprocess.run([sys.executable, '-m', 'pytest'] + suite,
                            cwd=str(checkout), text=True,
                            env=dict(os.environ, PYTHONPATH=str(checkout)),
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    (ROOT / (name + '-focused.log')).write_text(result.stdout)
    print(name, result.returncode, result.stdout.splitlines()[-1])

def observations(name):
    lines = (ROOT / (name + '-final-reproduction.log')).read_text().splitlines()
    return [{key: row[key] for key in ('case', 'actual', 'parameters',
                                     'full_parameters', 'ccn')}
            for row in map(json.loads, lines) if 'actual' in row]

cli = {}
for name in ('target', 'baseline'):
    checkout = CACHE / 'baseline' if name == 'baseline' else ROOT / name
    targets = [str(CACHE / 'corpus' / x) for x in
               ('lizard.py', 'lizard_core', 'lizard_languages', 'lizard_ext')]
    result = subprocess.run([sys.executable, str(checkout / 'lizard.py')] + targets,
                            cwd=str(checkout), stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE)
    assert result.returncode in (0, 1), result.stderr
    cli[name] = hashlib.sha256(result.stdout).hexdigest()
equivalence = dict(api=observations('target') == observations('baseline'),
                   broad_cli_hashes=cli, broad_cli=cli['target'] == cli['baseline'])
assert equivalence['api'] and equivalence['broad_cli'], equivalence
(ROOT / 'default-equivalence.json').write_text(json.dumps(equivalence, indent=2) + '\n')
print(json.dumps(equivalence))
