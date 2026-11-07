"""Freeze final hashes and run six incremental/cumulative paired workloads."""
import hashlib
import json
from pathlib import Path
import subprocess
import sys

REPO = Path(__file__).resolve().parents[4]
ROOT = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr535')
CACHE = ROOT.parent
product = ['lizard.py'] + [str(p.relative_to(ROOT / 'target'))
    for area in ('lizard_core', 'lizard_languages', 'lizard_ext')
    for p in sorted((ROOT / 'target' / area).rglob('*.py'))]
hashes = {}
for label, folder in [('target', ROOT / 'target'), ('corrected', ROOT / 'corrected'),
                      ('baseline', CACHE / 'baseline')]:
    hashes[label] = {name: hashlib.sha256((folder / name).read_bytes()).hexdigest()
                     for name in product}
for name in product:
    assert hashlib.sha256((REPO / name).read_bytes()).hexdigest() == hashes['corrected'][name]
changed = [name for name in product
           if hashes['target'][name] != hashes['corrected'][name]]
assert changed == ['lizard_languages/javascript_functions.py'], changed
(ROOT / 'snapshot-hashes.json').write_text(json.dumps(
    [{'path': name, **{label: values[name] for label, values in hashes.items()}}
     for name in product], separators=(',', ':')) + '\n')
for phase, baseline in [('incremental', ROOT / 'target'),
                        ('cumulative', CACHE / 'baseline')]:
    for workload, corpus in [('api', 'affected'), ('api-parameters', 'consumers'),
                             ('cli', 'affected')]:
        label = phase + '-' + workload
        headroom = {name: subprocess.check_output(command, text=True)
                    for name, command in [('uptime', ['uptime']),
                                          ('memory', ['vm_stat'])]}
        (ROOT / (label + '-headroom.json')).write_text(json.dumps(headroom, indent=2) + '\n')
        print(label, headroom['uptime'].strip(), flush=True)
        command = [sys.executable, str(ROOT / corpus / 'paired.py'),
            '--baseline', str(baseline), '--candidate', str(ROOT / 'corrected'),
            '--workload', workload, '--pairs', '20', '--seconds', '1.2',
            '--out', str(ROOT / (label + '.json'))]
        with (ROOT / (label + '-timings.log')).open('w') as log:
            result = subprocess.run(command, cwd='/private/tmp', stdout=log,
                                    stderr=subprocess.STDOUT)
        assert result.returncode == 0, label
        data = json.loads((ROOT / (label + '.json')).read_text())
        print(json.dumps({key: data[key] for key in
            ('workload', 'loops', 'medians', 'paired_95pct_ci', 'passes')}), flush=True)
