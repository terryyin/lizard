"""Sequence diagnostic raw and final incremental/cumulative proof."""
from pathlib import Path
import subprocess
import sys

ROOT = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr532')
workloads = [('raw-api', 'raw-target', 'affected', 'api'),
             ('raw-cli', 'raw-target', 'affected', 'cli'),
             ('corrected-api', 'corrected', 'affected', 'api'),
             ('corrected-parameters', 'corrected', 'consumers', 'api-parameters'),
             ('corrected-cli', 'corrected', 'affected', 'cli')]
for label, candidate, corpus, workload in workloads:
    print(label, subprocess.check_output(['uptime'], text=True).strip(), flush=True)
    command = [sys.executable, str(ROOT / corpus / 'paired.py'),
               '--baseline', str(ROOT / 'target'),
               '--candidate', str(ROOT / candidate), '--workload', workload,
               '--pairs', '20', '--seconds', '1.2', '--out', str(ROOT / (label + '.json'))]
    with (ROOT / (label + '-timings.log')).open('w') as log:
        result = subprocess.run(command, cwd='/private/tmp', stdout=log,
                                stderr=subprocess.STDOUT)
    assert result.returncode == 0, (label, result.returncode)
    print((ROOT / (label + '-timings.log')).read_text().splitlines()[-1], flush=True)
