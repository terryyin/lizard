"""Bounded profile of an unchanged-output block-bodied arrow control."""

import cProfile
import hashlib
import json
from pathlib import Path
import pstats
import sys

checkout, output = sys.argv[1:]
sys.path.insert(0, str(Path(checkout).resolve()))
from lizard import analyze_file

cases = json.loads(Path(__file__).with_name('inputs.json').read_text())
case = next(c for c in cases if c['filename'] == 'block_body.js')


def analyze():
    return analyze_file.analyze_source_code(case['filename'], case['source'])


observed = [[f.name, f.start_line, f.end_line, f.cyclomatic_complexity,
             f.nloc, f.token_count] for f in analyze().function_list]
assert observed == [['f', 1, 4, 2, 4, 12],
                    ['after', 5, 5, 1, 1, 7]], observed
for _ in range(20):
    analyze()
profile = cProfile.Profile()
profile.enable()
for _ in range(5000):
    analyze()
profile.disable()
stats = pstats.Stats(profile)
ranked = sorted(stats.stats.items(), key=lambda row: row[1][2], reverse=True)
rows = []
for (filename, line, name), (primitive, calls, own, cumulative, _) in ranked:
    if len(rows) < 15 or (filename.endswith(
            ('typescript_states.py', 'javascript_functions.py')) and name in
            ('__call__', '__init__', '_state_global', '_continues_expression',
             '_end_expression_body', '_opens_an_object')):
        rows.append(dict(file=filename, line=line, function=name, calls=calls,
                         self_seconds=own, cumulative_seconds=cumulative))
result = dict(checkout=checkout, loops=5000, observed_metrics=observed,
              source_sha256=hashlib.sha256(case['source'].encode()).hexdigest(),
              total_seconds=stats.total_tt, total_calls=stats.total_calls,
              selected_profile=rows)
Path(output).write_text(json.dumps(result, indent=2) + '\n')
print(json.dumps(result))
