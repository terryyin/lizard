"""Explain the measured affected-input cost without a changing source tree."""
import cProfile
import hashlib
import importlib
import io
import json
from pathlib import Path
import pstats
import sys

checkout = Path(sys.argv[1]).resolve()
sys.path.insert(0, str(checkout))
from lizard import analyze_file

paths = {}
for name in ('lizard', 'lizard_languages', 'lizard_languages.javascript_bindings',
             'lizard_languages.javascript_functions',
             'lizard_languages.typescript_states', 'lizard_core.function_info'):
    path = Path(importlib.import_module(name).__file__).resolve()
    assert checkout in path.parents, str(path)
    paths[name] = str(path)
root = checkout.parent / 'proof/corpus/affected'
selected = ['annotations.ts', 'defaults.js', 'destructured.js',
            'this-binding.ts', 'plain.js']
inputs = [(str(root / name), (root / name).read_text()) for name in selected]


def batch(loops):
    observations = []
    for _ in range(loops):
        observations = []
        for filename, source in inputs:
            result = analyze_file.analyze_source_code(filename, source)
            observations.append([[f.name, f.parameters, f.parameter_count,
                f.cyclomatic_complexity, f.nloc, f.token_count,
                f.start_line, f.end_line] for f in result.function_list])
    return hashlib.sha256(json.dumps(observations).encode()).hexdigest()


before = batch(2)
profiler = cProfile.Profile()
profiler.enable()
profiled = batch(500)
profiler.disable()
after = batch(2)
assert before == profiled == after
stream = io.StringIO()
stats = pstats.Stats(profiler, stream=stream).sort_stats('cumulative')
stats.print_stats(25)
stats.print_stats('javascript_bindings')
print(json.dumps({'checkout': str(checkout), 'paths': paths, 'unchanged_output': before,
                  'files': selected, 'loops': 500,
                  'scope': 'Valid named signatures whose outputs are unchanged within each before/after PR pair; _dec already owns parameter extraction.'}))
print(stream.getvalue())
