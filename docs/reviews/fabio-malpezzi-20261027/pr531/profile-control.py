"""Bounded profile with identical metrics for a working block-bodied arrow."""

import cProfile
import hashlib
import json
from pathlib import Path
import pstats
import sys

checkout, output = sys.argv[1:]
sys.path.insert(0, str(Path(checkout).resolve()))
from lizard import analyze_file
from lizard_ext.lizardns import LizardExtension

inputs = json.loads(Path(__file__).with_name('inputs.json').read_text())
source = next(c['source'] for c in inputs if c['filename'] == 'block_body.js')


def analyze():
    return analyze_file.analyze_source_code('control.js', source)


observed = [(f.name, f.cyclomatic_complexity, f.token_count, f.parameter_count,
             f.start_line, f.end_line, f.nloc) for f in analyze().function_list]
assert observed == [('make', 2, 11, 0, 2, 2, 1), ('outer', 1, 15, 1, 1, 4, 4),
                    ('after', 1, 7, 0, 5, 5, 1)], observed
for _ in range(20):
    analyze()
profile = cProfile.Profile()
profile.enable()
for _ in range(5000):
    analyze()
profile.disable()
stats = pstats.Stats(profile)
ranked = sorted(stats.stats.items(), key=lambda row: row[1][2], reverse=True)
selected = ('_opens_an_object', '_arrow_function', '_state_global', '__init__')
rows = []
for (filename, line, name), (primitive, calls, own, cumulative, _) in ranked:
    if len(rows) < 15 or (name in selected and filename.endswith(
            ('typescript_states.py', 'javascript_functions.py'))):
        rows.append({'file': filename, 'line': line, 'function': name,
                     'calls': calls, 'self_seconds': own,
                     'cumulative_seconds': cumulative})
result = {'checkout': checkout, 'loops': 5000, 'source': source,
          'source_sha256': hashlib.sha256(source.encode()).hexdigest(),
          'observed_metrics': observed, 'total_seconds': stats.total_tt,
          'total_calls': stats.total_calls, 'selected_profile': rows}
Path(output).write_text(json.dumps(result, indent=2) + '\n')
print(json.dumps(result))
