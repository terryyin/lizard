"""Observe the public analyzer from verified immutable source snapshots."""
import importlib
import json
from pathlib import Path
import sys

checkout = Path(sys.argv[1]).resolve()
sys.path.insert(0, str(checkout))
from lizard import analyze_file


def verify():
    for name in ('lizard', 'lizard_languages',
                 'lizard_languages.javascript_functions',
                 'lizard_languages.typescript_states',
                 'lizard_core.function_info'):
        module = importlib.import_module(name)
        path = Path(module.__file__).resolve()
        assert checkout == path or checkout in path.parents, str(path)


verify()
print(json.dumps({'checkout': str(checkout), 'facade': sys.modules['lizard'].__file__}))
failures = []
for case in json.loads(Path(sys.argv[2]).read_text()):
    result = analyze_file.analyze_source_code(case['filename'], case['source'])
    actual = [[f.name, f.parameter_count] for f in result.function_list]
    parameters = [f.parameters for f in result.function_list]
    passed = actual == case['expected'] and (
        'parameters' not in case or case['parameters'] == parameters)
    if not passed:
        failures.append(case['filename'])
    print(json.dumps({'case': case['filename'], 'expected': case['expected'],
        'actual': actual, 'parameters': parameters, 'passed': passed,
        'metrics': [[f.cyclomatic_complexity, f.nloc, f.token_count,
                    f.start_line, f.end_line] for f in result.function_list]}))
verify()
print(json.dumps({'cases': len(json.loads(Path(sys.argv[2]).read_text())),
                  'failures': failures}))
sys.exit(bool(failures))
