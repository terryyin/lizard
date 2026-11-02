"""Run the proposal's exact added assertions against this checkout's analyzer."""
import io
import json
from pathlib import Path
import sys
import types
import unittest

repo = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(repo))
import lizard
from lizard import analyze_file

assert Path(lizard.__file__).resolve() == repo / 'lizard.py'


root = Path(__file__).resolve().parent
snapshot = json.loads((root / 'github-snapshot.json').read_text())
suite = unittest.TestSuite()
for patch in snapshot['evidence'][0]['patches']:
    if patch['filename'] not in (
            'test/test_languages/testTypeScript.py',
            'test/test_languages/testTSX.py'):
        continue
    added = '\n'.join(line[1:] for line in patch['patch'].splitlines()
                      if line.startswith('+') and not line.startswith('+++'))
    added = added[added.index('class Test_'):]
    module = types.ModuleType(Path(patch['filename']).stem)
    module.__dict__.update(unittest=unittest)
    module.get_ts_function_list = lambda source: analyze_file.analyze_source_code(
        'a.ts', source).function_list
    module.get_tsx_function_list = lambda source: analyze_file.analyze_source_code(
        'a.tsx', source).function_list
    exec(compile(added, patch['filename'] + '@3b039e3', 'exec'), module.__dict__)
    suite.addTests(unittest.defaultTestLoader.loadTestsFromModule(module))

output = io.StringIO()
result = unittest.TextTestRunner(stream=output, verbosity=2).run(suite)
report = {'testsRun': result.testsRun,
          'failures': [{'test': test.id(), 'traceback': trace}
                       for test, trace in result.failures],
          'errors': [{'test': test.id(), 'traceback': trace}
                     for test, trace in result.errors],
          'output': output.getvalue()}
(root / 'pr519-added-tests-current.json').write_text(
    json.dumps(report, indent=2) + '\n')
print(output.getvalue())
