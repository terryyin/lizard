"""Observe PR534 public metrics with snapshot import isolation."""

import argparse
import inspect
import json
from pathlib import Path
import sys

parser = argparse.ArgumentParser()
parser.add_argument('--checkout', required=True)
parser.add_argument('--suite', choices=('authored', 'focused'))
parser.add_argument('--inputs', type=Path)
args = parser.parse_args()
checkout = Path(args.checkout).resolve()
sys.path.insert(0, str(checkout))
import lizard
import lizard_languages
from lizard import analyze_file, FileAnalyzer, get_extensions
from lizard_languages.javascript_functions import JavaScriptFunctionsMixin
from lizard_languages.typescript_states import TypeScriptStates
from test.test_languages import testES6

paths = dict(facade=lizard.__file__, registry=lizard_languages.__file__,
             functions=inspect.getfile(JavaScriptFunctionsMixin),
             states=inspect.getfile(TypeScriptStates), helper=testES6.__file__)
for path in paths.values():
    assert Path(path).resolve().is_relative_to(checkout), paths
print(json.dumps(dict(imports=paths)))
if args.suite:
    import pytest
    if args.suite == 'authored':
        selected = [str(Path(__file__).with_name('authored-tests.py'))]
    else:
        selected = sorted(str(p) for pattern in (
            'testJavaScript*.py', 'testTypeScript*.py', 'testES6.py',
            'testTSX.py', 'testVue.py')
                          for p in (checkout / 'test/test_languages').glob(pattern))
    raise SystemExit(pytest.main(['-q'] + selected))
inputs = json.loads((args.inputs or Path(__file__).with_name('inputs.json')).read_text())
failures = []
for case in inputs:
    functions = analyze_file.analyze_source_code(
        case['filename'], case['source']).function_list
    actual = [[f.name, f.start_line, f.end_line, f.cyclomatic_complexity,
               f.nloc, f.token_count] for f in functions]
    file_actual = [[f.name, f.start_line, f.end_line, f.cyclomatic_complexity,
                    f.nloc, f.token_count] for f in FileAnalyzer(
                        get_extensions([])).analyze_source_code(
                            case['filename'], case['source']).function_list]
    assert actual == file_actual, (case['filename'], actual, file_actual)
    expected = case['expected']
    passed = (None if expected is None else
              len(actual) == len(expected) and all(
                  row[:len(want)] == want for row, want in zip(actual, expected)))
    if passed is False:
        failures.append(case['filename'])
    print(json.dumps(dict(case=case['filename'], expected=expected,
                          actual=actual, passes=passed,
                          file_analyzer_matches=True)))
print(json.dumps(dict(checkout=str(checkout), cases=len(inputs),
                      failures=failures)))
raise SystemExit(1 if failures else 0)
