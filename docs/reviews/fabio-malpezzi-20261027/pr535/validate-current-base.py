"""Observe receiver counts through HTTP and validate the anticipated merge tree."""
import importlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys

HERE = Path(__file__).resolve().parent
REPO = HERE.parents[3]
CHECKOUT = Path('/private/tmp/lizard-pr532-validation')
ROOT = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr535')
BASE = '3a11bc6a5452be40a32721b5f6e2ab2fa787ca83'
HEAD = '171079e625966ac9c17aee29a5d74256befd85ae'
PRODUCT = 'lizard_languages/javascript_functions.py'
TEST = 'test/test_languages/testTypeScriptThisParameters.py'
AUTHORED = 'test/test_languages/testTypeScriptMetrics.py'

if len(sys.argv) > 1:
    sys.path.insert(0, str(CHECKOUT))
    from index import app
    paths = {}
    for name in ('index', 'lizard', 'lizard_languages',
                 'lizard_languages.javascript_functions',
                 'lizard_languages.typescript_states', 'lizard_core.function_info'):
        module = importlib.import_module(name)
        assert Path(module.__file__).resolve().is_relative_to(CHECKOUT), module.__file__
        paths[name] = module.__file__
    baseline = sys.argv[1] == 'baseline-http'
    cases = [('.ts', 'function onClick(this: HTMLElement, event: Event) { return event; }',
              'onClick', 2 if baseline else 1)]
    if not baseline:
        cases += [('.ts', 'const b = async function* (this: Reader) { yield 1; };', 'b', 0),
                  ('.js', 'function render(text, _key, $el) {}', 'render', 3),
                  ('.js', 'function unpack({first, this: receiver}, tail) {}', 'unpack', 2)]
    rows = []
    for language, source, name, count in cases:
        response = app.test_client().post('/analyze', json=dict(lang=language, code=source))
        assert response.status_code == 200, response.data
        function, = response.get_json()['function_list']
        assert (function['name'], function['parameter_count']) == (name, count), function
        rows.append(dict(name=name, count=function['parameter_count'], status=response.status_code))
    print(json.dumps(dict(baseline_observation=baseline, imports=paths, observations=rows)))
    sys.exit(0)

def git(*args):
    return subprocess.check_output(['git', '-C', str(CHECKOUT)] + list(args), text=True).strip()

def run(label, command):
    result = subprocess.run(command, cwd=CHECKOUT, text=True,
                            env=dict(os.environ, PYTHONPATH=str(CHECKOUT)),
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    (ROOT / (label + '.log')).write_text(result.stdout)
    lines = [line.rstrip() for line in result.stdout.splitlines()]
    for offset in range(0, len(lines), 230):
        suffix = '' if len(lines) <= 230 else '.part' + str(offset // 230 + 1)
        (HERE / (label + '.log' + suffix)).write_text(
            '\n'.join(lines[offset:offset + 230]).rstrip() + '\n')
    assert result.returncode == 0, (label, result.stdout[-3000:])
    return result.stdout

assert git('rev-parse', 'HEAD') == BASE
before = json.loads(run('current-base-http-before', [sys.executable, __file__, 'baseline-http']))
for name in (PRODUCT, TEST):
    shutil.copyfile(REPO / name, CHECKOUT / name)
assert (CHECKOUT / PRODUCT).read_bytes() == (ROOT / 'corrected' / PRODUCT).read_bytes()
(CHECKOUT / AUTHORED).write_bytes(subprocess.check_output(
    ['git', '-C', str(REPO), 'show', HEAD + ':' + AUTHORED]))
tests = run('current-base-pytest', [sys.executable, '-m', 'pytest'])
checks = run('current-base-make', ['make'])
after = json.loads(run('current-base-http-after', [sys.executable, __file__, 'http']))
git('add', '--', PRODUCT, TEST, AUTHORED)
record = dict(base=BASE, original_head=HEAD, anticipated_merge_tree=git('write-tree'),
              http_before=before, http_after=after,
              pytest=[line for line in tests.splitlines() if ' passed' in line],
              make=[line for line in checks.splitlines() if ' passed' in line or 'rated at' in line])
(HERE / 'current-base-proof.json').write_text(json.dumps(record, indent=2) + '\n')
print(json.dumps(record), flush=True)
