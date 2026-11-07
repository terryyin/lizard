"""Assert PR531 object-arrow and valid function-type boundaries through the API."""

import argparse
import json
from pathlib import Path
import sys

parser = argparse.ArgumentParser()
parser.add_argument('--checkout', required=True)
args = parser.parse_args()
sys.path.insert(0, str(Path(args.checkout).resolve()))
from lizard import analyze_file

inputs = json.loads(Path(__file__).with_name('inputs.json').read_text())
failures = []
for case in inputs:
    functions = analyze_file.analyze_source_code(
        case['filename'], case['source']).function_list
    actual = [[f.name, f.start_line, f.end_line, f.cyclomatic_complexity]
              for f in functions]
    passed = actual == case['expected']
    if not passed:
        failures.append(case['filename'])
    print(json.dumps({'case': case['filename'], 'expected': case['expected'],
                      'actual': actual, 'passes': passed,
                      'metrics': [[f.name, f.nloc, f.token_count,
                                   f.parameter_count] for f in functions]}))
print(json.dumps({'checkout': args.checkout, 'cases': len(inputs),
                  'failures': failures}))
raise SystemExit(1 if failures else 0)
