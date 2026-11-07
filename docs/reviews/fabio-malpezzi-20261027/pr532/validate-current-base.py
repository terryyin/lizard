"""Validate the complete anticipated PR merge tree on the current trunk."""
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys

HERE = Path(__file__).resolve().parent
REPO = HERE.parents[3]
CHECKOUT = Path('/private/tmp/lizard-pr532-validation')
ROOT = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr532')
BASE = 'd8e8e9af75b6d05da005e981af5f8867cced5d55'
HEAD = 'd0713bf5b4dcf4bf24453156a3756688627abbc0'
PRODUCT = ['lizard_core/function_info.py',
           'lizard_languages/javascript_bindings.py',
           'lizard_languages/javascript_functions.py',
           'lizard_languages/typescript.py', 'lizard_languages/tsx.py']
TEST = 'test/test_languages/testJavaScriptParameterNames.py'
AUTHORED = ['test/test_languages/testJavaScriptFunctions.py',
            'test/test_languages/testTypeScriptMetrics.py']

def git(*args):
    return subprocess.check_output(['git', '-C', str(CHECKOUT)] + list(args), text=True).strip()

assert git('rev-parse', 'HEAD') == BASE
for name in PRODUCT + [TEST]:
    shutil.copyfile(REPO / name, CHECKOUT / name)
for name in AUTHORED:
    (CHECKOUT / name).write_bytes(subprocess.check_output(
        ['git', '-C', str(REPO), 'show', HEAD + ':' + name]))

def retain(name, text):
    (ROOT / name).write_text(text)
    lines = [line.rstrip() for line in text.splitlines()]
    for offset in range(0, len(lines), 230):
        suffix = '' if len(lines) <= 230 else '.part' + str(offset // 230 + 1)
        (HERE / (name + suffix)).write_text('\n'.join(lines[offset:offset + 230]).rstrip() + '\n')

commands = [[sys.executable, '-m', 'pytest'], ['make']]
summaries = []
for label, command in zip(('pytest', 'make'), commands):
    result = subprocess.run(command, cwd=CHECKOUT, text=True,
                            env=dict(os.environ, PYTHONPATH=str(CHECKOUT)),
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    retain('current-base-' + label + '.log', result.stdout)
    assert result.returncode == 0, (label, result.stdout[-3000:])
    summaries.append(dict(command=command, exit=result.returncode,
                          result=[line for line in result.stdout.splitlines()
                                  if ' passed' in line or 'rated at' in line]))
result = subprocess.run([sys.executable, str(HERE / 'observe-website.py'), str(CHECKOUT)],
                        cwd='/private/tmp', text=True, stdout=subprocess.PIPE,
                        stderr=subprocess.STDOUT)
retain('website-integrated.log', result.stdout)
assert result.returncode == 0, result.stdout
http = json.loads(result.stdout)
git('add', '--', *(PRODUCT + [TEST] + AUTHORED))
tree = git('write-tree')
record = dict(base=BASE, original_head=HEAD, anticipated_merge_tree=tree,
              changed_paths=PRODUCT + [TEST] + AUTHORED,
              commands=summaries, http=http)
(HERE / 'current-base-proof.json').write_text(json.dumps(record, indent=2) + '\n')
print(json.dumps(record), flush=True)
