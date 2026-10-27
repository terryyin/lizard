"""Short causal profile and calibration, never an acceptance experiment."""
import cProfile
import json
import os
from pathlib import Path
import pstats
import statistics
import subprocess
import sys
import time

ROOT = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr536-corrected')
if len(sys.argv) == 1:
    print(json.dumps({'cores': os.cpu_count(), 'load': os.getloadavg(),
                      'memory': subprocess.check_output(['vm_stat'], text=True),
                      'time': time.time(), 'executable': sys.executable}), flush=True)
    for label in ['baseline', 'target', 'candidate-0']:
        result = subprocess.run([sys.executable, __file__, label],
                                cwd='/private/tmp', capture_output=True, text=True)
        (ROOT / (label + '-diagnose.log')).write_text(result.stdout + result.stderr)
        print(label, result.returncode, result.stdout[:500], flush=True)
        assert result.returncode == 0
    sys.exit(0)

os.chdir('/private/tmp')
snapshot = ROOT / sys.argv[1]
sys.path.insert(0, str(snapshot))
from lizard import analyze_file
from lizard_ext.lizardns import LizardExtension


def guard():
    for name in ['lizard', 'lizard_core.function_info',
                 'lizard_languages.javascript_bindings',
                 'lizard_languages.javascript_functions',
                 'lizard_languages.typescript_states', 'lizard_ext.lizardns']:
        module = __import__(name, fromlist=['__file__'])
        assert snapshot in Path(module.__file__).resolve().parents


guard()
cases = [(str(p), p.read_text()) for p in sorted((ROOT / 'proof/corpus/affected').iterdir())]


def batch(loops):
    start = time.perf_counter()
    for _ in range(loops):
        for name, source in cases:
            result = analyze_file.analyze_source_code(name, source)
            for function in result.function_list:
                function.parameters
                function.parameter_count
    return time.perf_counter() - start


batch(3)
durations = [batch(10) for _ in range(5)]
print(json.dumps({'snapshot': str(snapshot), 'calibration_10loops': durations,
                  'median': statistics.median(durations)}), flush=True)
profile = cProfile.Profile()
profile.runcall(batch, 15)
profile.dump_stats(str(ROOT / (sys.argv[1] + '.pstats')))
pstats.Stats(profile).strip_dirs().sort_stats('tottime').print_stats(25)
guard()
