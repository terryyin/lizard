"""Observe immutable snapshots through analyzer and actual CLI boundaries."""
import importlib
import json
import os
from pathlib import Path
import subprocess
import sys

checkout = Path(sys.argv[1]).resolve()
sys.path.insert(0, str(checkout))
from lizard import analyze_file


def verify():
    result = {}
    for name in ('lizard', 'lizard_languages', 'lizard_languages.javascript_bindings',
                 'lizard_languages.javascript_functions',
                 'lizard_languages.typescript_states', 'lizard_core.function_info'):
        path = Path(importlib.import_module(name).__file__).resolve()
        assert checkout in path.parents, str(path)
        result[name] = str(path)
    return result


print(json.dumps({'setup': verify()}))
cases = json.loads(Path(sys.argv[2]).read_text())
failures = []
for case in cases:
    functions = analyze_file.analyze_source_code(
        case['filename'], case['source']).function_list
    counts = [[f.name, f.parameter_count] for f in functions]
    parameters = [f.parameters for f in functions]
    metrics = [[f.cyclomatic_complexity, f.nloc, f.token_count,
                f.start_line, f.end_line] for f in functions]
    passed = (counts == case['expected']
        and ('parameters' not in case or parameters == case['parameters'])
        and ('ccn' not in case or [f.cyclomatic_complexity for f in functions] == case['ccn']))
    if not passed:
        failures.append(case['filename'])
    print(json.dumps({'case': case['filename'], 'actual': counts,
        'parameters': parameters, 'metrics': metrics, 'passed': passed,
        'expected': case['expected']}))
reported = Path(sys.argv[2]).parent / 'proof/corpus/affected/arrow-reported.ts'
reported_case = next(case for case in cases if case['filename'] == 'arrow-reported.ts')
assert reported.is_file()
assert reported.read_text() == reported_case['source']
reported_functions = analyze_file.analyze_source_code(
    str(reported), reported_case['source']).function_list
assert len(reported_functions) == 4
command = [sys.executable, str(checkout / 'lizard.py'), str(reported)]
result = subprocess.run(command, cwd='/private/tmp',
    env=dict(os.environ, PYTHONPATH=str(checkout)), text=True,
    stdout=subprocess.PIPE, stderr=subprocess.PIPE)
print(json.dumps({'cli_command': command, 'returncode': result.returncode,
                  'stdout': result.stdout, 'stderr': result.stderr}))
assert result.returncode in (0, 1)
rows = [line for line in result.stdout.splitlines()
        if '(anonymous)@' in line and str(reported) in line]
assert len(rows) == 4, result.stdout
for row, function in zip(rows, reported_functions):
    columns = row.split()
    assert list(map(int, columns[:5])) == [function.nloc,
        function.cyclomatic_complexity, function.token_count,
        function.parameter_count, function.length], row
    assert columns[-1] == ('(anonymous)@' + str(function.start_line)
        + '-' + str(function.end_line) + '@' + str(reported)), row
print(json.dumps({'verified_after': verify(), 'failures': failures,
                  'cases': len(cases)}))
sys.exit(bool(failures))
