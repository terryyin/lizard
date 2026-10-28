"""Measure each invalid proposal once; retain every pair and host observation."""
import json
import os
from pathlib import Path
import subprocess
import sys
import time

ROOT = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr536')
decisions = {
    'time': time.time(), 'pid': os.getpid(),
    'raw': 'parent01afe9c versus full authored92fff8a; no accepted-set dependency assumed',
    'adapted': 'corrected current targetd7be8b9 versus only actual PR536 patch',
    'api': '20 alternating warmed pairs calibrated to1.2s; all65 fixed inputs',
    'api-parameters': 'same corpus; explicit public names/count properties observed',
    'cli': '20 alternating warmed pairs calibrated to4s on original100 Python files; longer batches chosen prospectively after535 CLI noise',
    'retry': 'No invalid-proposal retry to hunt a pass. Failure/noise is retained.',
    'cumulative': 'No536 code is to land: accepted535 cumulative proof remains valid unchanged; raw/adapted measurements characterize deferred proposal only.',
}
(ROOT / 'measurement-decision.json').write_text(json.dumps(decisions, indent=2) + '\n')
for label, baseline, candidate in [('raw', 'parent', 'raw'),
                                   ('adapted', 'target', 'adapted')]:
    for workload in ('api', 'api-parameters', 'cli'):
        prefix = label + '-' + workload
        observations = {}
        for name, command in [('uptime', ['uptime']),
            ('cpu', ['sysctl', '-n', 'hw.logicalcpu']),
            ('memory', ['memory_pressure']),
            ('processes', ['ps', '-Ao', 'pid,pcpu,pmem,comm'])]:
            result = subprocess.run(command, text=True, stdout=subprocess.PIPE)
            observations[name] = result.stdout
        observations['decision'] = '16logical cores, one proof process, parent-owned heavy work idle; measure once and retain uncertainty'
        (ROOT / (prefix + '-headroom.json')).write_text(json.dumps(observations) + '\n')
        command = [sys.executable, str(ROOT / 'proof/paired.py'),
            '--baseline', str(ROOT / baseline), '--candidate', str(ROOT / candidate),
            '--workload', workload, '--pairs', '20', '--seconds',
            '4' if workload == 'cli' else '1.2', '--out', str(ROOT / (prefix + '.json'))]
        print(json.dumps({'starting': prefix, 'command': command}), flush=True)
        with (ROOT / (prefix + '-timings.log')).open('w') as output:
            result = subprocess.run(command, cwd='/private/tmp', stdout=output,
                                    stderr=subprocess.STDOUT)
        assert result.returncode == 0, prefix
        outcome = json.loads((ROOT / (prefix + '.json')).read_text())
        print(json.dumps({'finished': prefix, 'median': outcome['median_slowdown_pct'],
            'ci': outcome['paired_95pct_ci'], 'passes': outcome['passes'],
            'shortest_batch': min(min(row.values()) for row in outcome['raw'])}), flush=True)
