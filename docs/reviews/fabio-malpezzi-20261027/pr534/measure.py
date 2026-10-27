"""Run one paired diagnostic comparison and one unchanged-output profile."""
import json
from pathlib import Path
import shutil
import subprocess
import sys

HERE = Path(__file__).resolve().parent
CACHE = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534')
for workload in ('api', 'cli'):
    command = [sys.executable, str(CACHE / 'paired.py'),
               '--baseline', str(CACHE / 'target'),
               '--candidate', str(CACHE / 'raw'), '--workload', workload,
               '--pairs', '20', '--seconds', '1.5',
               '--out', str(CACHE / ('534-' + workload + '.json'))]
    with (CACHE / ('534-' + workload + '-terminal.log')).open('w') as log:
        result = subprocess.run(command, cwd=CACHE / 'corpus', stdout=log,
                                stderr=subprocess.STDOUT)
    print(workload, 'exit', result.returncode, flush=True)
    result.check_returncode()
    data = json.loads((CACHE / ('534-' + workload + '.json')).read_text())
    print(json.dumps({key: data[key] for key in
                      ('medians', 'stdev', 'median_slowdown_pct',
                       'paired_95pct_ci', 'passes')}), flush=True)
shutil.copy2(HERE / 'profile-control.py', CACHE / 'profile-control.py')
for snapshot in ('target', 'raw'):
    command = [sys.executable, str(CACHE / 'profile-control.py'),
               str(CACHE / snapshot), str(CACHE / (snapshot + '-profile.json'))]
    with (CACHE / (snapshot + '-profile.log')).open('w') as log:
        result = subprocess.run(command, cwd=CACHE / 'corpus', stdout=log,
                                stderr=subprocess.STDOUT)
    print(snapshot, 'profile exit', result.returncode, flush=True)
    result.check_returncode()
