"""Observe parameter names and counts through an isolated public analyzer."""
import importlib
import json
from pathlib import Path
import sys

checkout = Path(sys.argv[1]).resolve()
sys.path.insert(0, str(checkout))
from lizard import analyze_file

for name in ('lizard', 'lizard_languages',
             'lizard_languages.javascript_functions',
             'lizard_languages.typescript_states',
             'lizard_core.function_info', 'test.test_languages.testTypeScript'):
    module = importlib.import_module(name)
    assert Path(module.__file__).resolve().is_relative_to(checkout), module.__file__
    print(json.dumps({'module': name, 'path': module.__file__}))

inputs = json.loads(Path(sys.argv[2]).read_text())
failures = []
for case in inputs:
    functions = analyze_file.analyze_source_code(
        case['filename'], case['source']).function_list
    actual = [[f.name, f.parameter_count] for f in functions]
    passed = actual == case['expected']
    if not passed:
        failures.append(case['filename'])
    print(json.dumps(dict(case=case['filename'], expected=case['expected'],
                         actual=actual, passed=passed,
                         parameters=[f.parameters for f in functions],
                         full_parameters=[f.full_parameters for f in functions],
                         ccn=[f.cyclomatic_complexity for f in functions])))
print(json.dumps({'cases': len(inputs), 'failures': failures}))
sys.exit(bool(failures))
