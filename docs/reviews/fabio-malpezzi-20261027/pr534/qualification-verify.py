"""Observe frozen snapshots through real public analysis before timing."""

import importlib
import inspect
import json
from pathlib import Path
import sys

snapshot = Path(sys.argv[1]).resolve()
inputs = Path(sys.argv[2]).resolve()
sys.path.insert(0, str(snapshot))
from lizard import analyze_file, FileAnalyzer, get_extensions
from lizard_languages.javascript_functions import JavaScriptFunctionsMixin
from lizard_languages.typescript_states import TypeScriptStates
from test.test_languages import testES6

modules = ['lizard', 'lizard_languages', 'lizard_core.file_analyzer',
           'lizard_languages.typescript', 'lizard_languages.javascript_bindings']
if (snapshot / 'lizard_languages/typescript_signatures.py').exists():
    modules.append('lizard_languages.typescript_signatures')
paths = {name: importlib.import_module(name).__file__ for name in modules}
paths.update(functions=inspect.getfile(JavaScriptFunctionsMixin),
             states=inspect.getfile(TypeScriptStates), helper=testES6.__file__)
assert all(Path(path).resolve().is_relative_to(snapshot) for path in paths.values()), paths
print(json.dumps(dict(imports=paths)), flush=True)
failures = []
cases = json.loads(inputs.read_text())
for case in cases:
    functions = analyze_file.analyze_source_code(case['filename'], case['source']).function_list
    fields = ('name', 'start_line', 'end_line', 'cyclomatic_complexity',
              'nloc', 'token_count', 'parameter_count')
    actual = [[getattr(function, field) for field in fields] for function in functions]
    independent = FileAnalyzer(get_extensions([])).analyze_source_code(
        case['filename'], case['source']).function_list
    assert actual == [[getattr(function, field) for field in fields]
                      for function in independent], case['filename']
    expected = case['expected']
    passed = (expected is None or len(actual) == len(expected) and all(
        row[:len(want)] == want for row, want in zip(actual, expected)))
    if not passed:
        failures.append(case['filename'])
    print(json.dumps(dict(case=case['filename'], expected=expected,
                          actual=actual, passes=passed)))
assert all(Path(path).resolve().is_relative_to(snapshot) for path in paths.values())
print(json.dumps(dict(snapshot=str(snapshot), cases=len(cases), failures=failures)))
if snapshot.name == 'candidate' or '--require-expected' in sys.argv:
    assert not failures, failures
