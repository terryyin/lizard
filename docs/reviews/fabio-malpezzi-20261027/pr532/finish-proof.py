"""Retain final bounded diagnostics and check owned evidence files."""
import json
from pathlib import Path
import runpy
import subprocess
import sys

REPO = Path(__file__).resolve().parents[4]
HERE = Path(__file__).resolve().parent
ROOT = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr532')
result = subprocess.run([sys.executable, str(HERE / 'observe-website.py')],
                        cwd=str(REPO), text=True, stdout=subprocess.PIPE,
                        stderr=subprocess.STDOUT)
assert result.returncode == 0, result.stdout
print(result.stdout)
for name, checkout, source, label in [
        ('baseline', ROOT.parent / 'baseline', 'supplemental.json', 'supplemental'),
        ('target', ROOT / 'target', 'preexisting.json', 'preexisting'),
        ('raw-target', ROOT / 'raw-target', 'preexisting.json', 'preexisting')]:
    command = [sys.executable, str(HERE / 'reproduce.py'), str(checkout), str(HERE / source)]
    result = subprocess.run(command, cwd='/private/tmp', text=True,
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    (ROOT / (name + '-' + label + '.log')).write_text(result.stdout)
    assert result.returncode == 1, result.stdout
    print(name, label, result.stdout.splitlines()[-2:])
runpy.run_path(str(HERE / 'retain-evidence.py'), run_name='__main__')
changed = subprocess.check_output(['git', 'diff', '--name-only'], cwd=REPO, text=True).splitlines()
untracked = subprocess.check_output(['git', 'ls-files', '--others', '--exclude-standard'], cwd=REPO, text=True).splitlines()
owned = [path for path in changed + untracked if
         path.startswith('docs/reviews/fabio-malpezzi-20261027/') or path in (
             '.planning/slice-plans/004-fabio-malpezzi-pr-reviews/532-review.md',
             '.planning/slice-plans/004-fabio-malpezzi-pr-reviews/533-review.md',
             'test/test_languages/testJavaScriptParameterNames.py',
             'lizard_core/function_info.py', 'lizard_languages/javascript_bindings.py',
             'lizard_languages/javascript_functions.py', 'lizard_languages/typescript.py',
             'lizard_languages/tsx.py')]
issues = []
for name in owned:
    lines = (REPO / name).read_text().splitlines()
    if len(lines) > 250:
        issues.append((name, 'lines', len(lines)))
    issues.extend((name, 'whitespace', index) for index, line in enumerate(lines, 1)
                  if line.rstrip() != line)
assert not issues, issues
payload = dict(owned_paths_checked=len(owned), line_limit=250, whitespace='pass')
(HERE / 'final-evidence-check.json').write_text(json.dumps(payload) + '\n')
print(json.dumps(payload))
