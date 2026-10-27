"""One bounded profile on a block-arrow input with unchanged public output."""
import cProfile
import hashlib
import inspect
import json
from pathlib import Path
import pstats
import sys

checkout, output = sys.argv[1:]
checkout = Path(checkout).resolve()
sys.path.insert(0, str(checkout))
import lizard
import lizard_languages
from lizard import analyze_file
from lizard_languages.javascript_functions import JavaScriptFunctionsMixin
from lizard_languages.typescript_states import TypeScriptStates
paths = [lizard.__file__, lizard_languages.__file__,
         inspect.getfile(JavaScriptFunctionsMixin), inspect.getfile(TypeScriptStates)]
assert all(Path(path).resolve().is_relative_to(checkout) for path in paths), paths
case = next(c for c in json.loads(Path(__file__).with_name('inputs.json').read_text())
            if c['filename'] == 'block_branches.js')

def analyze():
    return analyze_file.analyze_source_code(case['filename'], case['source'])

observed = [[f.name, f.start_line, f.end_line, f.cyclomatic_complexity,
             f.nloc, f.token_count] for f in analyze().function_list]
assert observed == [['f', 2, 2, 1, 1, 5], ['(anonymous)', 2, 2, 2, 1, 7],
                    ['outer', 1, 4, 2, 4, 23], ['after', 5, 5, 1, 1, 7]], observed
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
            ('__call__', '__init__', '_state_global', '_arrow_function', '_opens_an_object')):
        rows.append(dict(file=filename, line=line, function=name, calls=calls,
                         self_seconds=own, cumulative_seconds=cumulative))
result = dict(checkout=str(checkout), loops=5000, imports=paths,
              observed_metrics=observed,
              source_sha256=hashlib.sha256(case['source'].encode()).hexdigest(),
              total_seconds=stats.total_tt, total_calls=stats.total_calls,
              selected_profile=rows)
Path(output).write_text(json.dumps(result, indent=2) + '\n')
print(json.dumps(dict(checkout=str(checkout), seconds=stats.total_tt,
                      calls=stats.total_calls)))
