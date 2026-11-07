"""Bounded profile of an unchanged public analysis result, without regex literals."""

import cProfile
import hashlib
import json
from pathlib import Path
import pstats
import sys

checkout, output = sys.argv[1:]
sys.path.insert(0, str(Path(checkout).resolve()))
from lizard import analyze_file

source = 'function divide(x,a,b,c) { return x / (a || b) / c; }'

def analyze():
    return analyze_file.analyze_source_code('control.js', source)

def metrics():
    return [(f.name, f.cyclomatic_complexity, f.token_count, f.parameter_count,
             f.start_line, f.end_line, f.nloc) for f in analyze().function_list]

observed = metrics()
assert observed == [('divide', 2, 22, 4, 1, 1, 1)], observed
for _ in range(20):
    analyze()
profile = cProfile.Profile()
profile.enable()
for _ in range(5000):
    analyze()
profile.disable()
stats = pstats.Stats(profile)
ranked = sorted(stats.stats.items(), key=lambda row: row[1][2], reverse=True)
top = []
for (filename, line, name), (primitive, calls, own, cumulative, callers) in ranked[:15]:
    top.append({'file': filename, 'line': line, 'function': name,
                'calls': calls, 'self_seconds': own, 'cumulative_seconds': cumulative})
result = {'checkout': checkout, 'loops': 5000, 'source': source,
          'source_sha256': hashlib.sha256(source.encode()).hexdigest(),
          'observed_metrics': observed, 'total_seconds': stats.total_tt,
          'total_calls': stats.total_calls, 'top_by_self_time': top}
Path(output).write_text(json.dumps(result, indent=2) + '\n')
print(json.dumps(result))
