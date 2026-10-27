"""One bounded unchanged-output profile, isolated from legacy profile.py."""

import json
import os
from pathlib import Path
import shutil
import subprocess
import sys

if '--worker' not in sys.argv:
    temporary = Path('/private/tmp/lizard-seed015-qualification-profile.py')
    shutil.copyfile(__file__, temporary)
    for snapshot in ('current', 'candidate'):
        command = [sys.executable, str(temporary), '--worker', snapshot]
        subprocess.run(command, cwd='/private/tmp', check=True)
    raise SystemExit(0)

import cProfile
import profile
import pstats

CACHE = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-performance')
snapshot = CACHE / sys.argv[2]
assert not Path(profile.__file__).resolve().is_relative_to(snapshot)
sys.path.insert(0, str(snapshot))
import lizard

assert Path(lizard.__file__).resolve().is_relative_to(snapshot)
case = next(case for case in json.loads((CACHE / 'inputs.json').read_text())
            if case['filename'] == 'block_branches.js')
fields = ('name', 'start_line', 'end_line', 'cyclomatic_complexity',
          'nloc', 'token_count', 'parameter_count')


def analyze():
    return lizard.analyze_file.analyze_source_code(case['filename'], case['source'])


actual = [[getattr(function, field) for field in fields]
          for function in analyze().function_list]
expected = case['expected']
assert len(actual) == len(expected)
assert all(row[:len(want)] == want for row, want in zip(actual, expected))
print(json.dumps(dict(profile=sys.argv[2], host=os.uname().nodename,
                      load=os.getloadavg(), loops=5000, actual=actual,
                      imports=dict(facade=lizard.__file__, profile=profile.__file__))), flush=True)
for _ in range(100):
    analyze()
profiler = cProfile.Profile()
profiler.enable()
for _ in range(5000):
    analyze()
profiler.disable()
stats = pstats.Stats(profiler)
rows = []
for (file, line, name), values in stats.stats.items():
    calls, total_calls, own, cumulative, _ = values
    rows.append(dict(file=file, line=line, function=name, calls=calls,
                     total_calls=total_calls, own_seconds=own,
                     cumulative_seconds=cumulative))
rows.sort(key=lambda row: row['own_seconds'], reverse=True)
payload = dict(snapshot=str(snapshot), loops=5000, actual=actual,
               total_seconds=stats.total_tt, total_calls=stats.total_calls,
               rows=rows)
(CACHE / (sys.argv[2] + '-profile.json')).write_text(json.dumps(payload, indent=2) + '\n')
print(json.dumps(dict(snapshot=sys.argv[2], total_seconds=stats.total_tt,
                      hot=rows[:12])), flush=True)
