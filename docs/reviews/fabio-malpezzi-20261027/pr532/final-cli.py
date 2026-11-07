"""One prospectively fixed ordinary CLI run in the observed quieter window."""
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys

HERE = Path(__file__).resolve().parent
REPO = HERE.parents[3]
ROOT = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr532')
PRODUCT = ['lizard_core/function_info.py',
           'lizard_languages/javascript_bindings.py',
           'lizard_languages/javascript_functions.py',
           'lizard_languages/typescript.py', 'lizard_languages/tsx.py']
for name in PRODUCT:
    assert (REPO / name).read_bytes() == (ROOT / 'corrected' / name).read_bytes(), name
harness = ROOT / 'affected/paired.py'
assert harness.read_bytes() == (HERE.parent / 'paired.py').read_bytes()
manifest = json.loads((ROOT / 'affected/manifest.json').read_text())
for name, expected in manifest.items():
    assert hashlib.sha256((ROOT / 'affected/corpus' / name).read_bytes()).hexdigest() == expected, name

command = [sys.executable, str(harness), '--baseline', str(ROOT / 'target'),
           '--candidate', str(ROOT / 'corrected'), '--workload', 'cli',
           '--pairs', '40', '--seconds', '8',
           '--out', str(ROOT / 'corrected-cli-quiet.json')]
decision = {
    'owner': 'root', 'command': command,
    'host': os.uname().nodename, 'cpus': os.cpu_count(),
    'launch_load': os.getloadavg(),
    'harness_sha256': hashlib.sha256(harness.read_bytes()).hexdigest(),
    'prior_result': 'corrected-cli-long.json: upper 5.67037%; inconclusive',
    'new_evidence': 'Later ENS run log-ratio SD 2.04247% versus ordinary 10.99303%; '
                    'fresh load 10.99/10.82/13.17 on 16 CPUs versus prior 23.84/24.77/19.58.',
    'choice': '40 alternating pairs, calibrated to 8 seconds; retain every pair and '
              'judge only the terminal interval. No owned heavy work competes.',
    'gate': 'Upper paired 95% slowdown bound strictly below 3%. '
            'The fixed t(19) critical value is conservative for 40 pairs.',
    'earlier_runs': 'Preserved without relabeling; improved host conditions justify this run.',
}
(HERE / 'final-cli-decision.json').write_text(json.dumps(decision, indent=2) + '\n')
with (HERE / 'corrected-cli-quiet-timings.log').open('w') as log:
    result = subprocess.run(command, cwd='/private/tmp', stdout=log,
                            stderr=subprocess.STDOUT)
assert result.returncode == 0, result.returncode
data = json.loads((ROOT / 'corrected-cli-quiet.json').read_text())
(HERE / 'corrected-cli-quiet.json').write_text(json.dumps(data, separators=(',', ':')) + '\n')
print(json.dumps({key: data[key] for key in (
    'loops', 'medians', 'stdev', 'median_slowdown_pct',
    'paired_geometric_slowdown_pct', 'paired_95pct_ci', 'passes')}), flush=True)
