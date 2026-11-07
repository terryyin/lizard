"""Run reader/metric consumers and retain installed whole-checkout traces."""
import json
from pathlib import Path
import subprocess
import sys

REPO = Path(__file__).resolve().parents[4]
ROOT = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr535')
trace = '.agents/skills/dough-execute-plan/scripts/consumer-trace.mjs'
terms = [
    ['--path', 'lizard_languages/javascript_functions.py',
     '--term', 'JavaScriptFunctionsMixin', '--term', '_read_parameter_binding'],
    ['--term', 'TypeScriptReader', '--term', 'TSXReader',
     '--term', 'JavaScriptReader', '--term', 'VueReader',
     '--term', 'get_js_function_list', '--term', 'get_ts_function_list',
     '--term', 'parameter_count'],
]
for index, arguments in enumerate(terms, 1):
    command = ['node', trace, '--checkout', str(REPO)] + arguments
    result = subprocess.run(command, cwd=REPO, text=True,
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    (ROOT / ('consumer-trace-' + str(index) + '.md')).write_text(result.stdout)
    assert result.returncode == 0, result.stdout
    print(json.dumps({'trace': index, 'lines': len(result.stdout.splitlines()),
                      'command': command}), flush=True)
tests = sorted(str(p.relative_to(REPO)) for pattern in
    ('testJavaScript*.py', 'testTypeScript*.py', 'testES6.py', 'testTSX.py',
     'testVue.py') for p in (REPO / 'test/test_languages').glob(pattern))
tests += ['test/test_extensions', 'test/test_analyzer.py', 'test/testOutput.py',
          'test/testApplication.py', 'test/test_options.py']
command = [sys.executable, '-m', 'pytest', '-q'] + tests
result = subprocess.run(command, cwd=REPO, text=True,
                        stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
(ROOT / 'focused.log').write_text(result.stdout)
(ROOT / 'focused-command.json').write_text(json.dumps(command, indent=2) + '\n')
print(json.dumps({'command': command, 'returncode': result.returncode,
                  'summary': result.stdout.splitlines()[-1]}), flush=True)
assert result.returncode == 0, result.stdout
