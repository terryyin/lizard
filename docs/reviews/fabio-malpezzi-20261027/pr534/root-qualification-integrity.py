"""Check evidence integrity without repeating setup, analysis or measurement."""
import ast
import hashlib
import json
from pathlib import Path
import subprocess
from root_qualification_common import CACHE, CHECKOUT, HERE, assert_inputs, now, write_json


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


assert_inputs()
assert subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=CHECKOUT, text=True).strip() == \
    '737aeaf7334b8e64f6ab0011c0f2ea65c356b06d'
plan = '.planning/slice-plans/011-ternary-arrow-boundaries/PLAN.md'
tracked = subprocess.check_output(['git', 'diff', 'HEAD', '--name-only'],
                                  cwd=CHECKOUT, text=True).splitlines()
assert set(tracked) <= {plan}, tracked
paths = subprocess.check_output(['git', 'ls-files', '--others', '--exclude-standard'],
                                cwd=CHECKOUT, text=True).splitlines()
assert paths and all(name.startswith(str(HERE.relative_to(CHECKOUT)) + '/root') for name in paths)
output = HERE / 'root-integrity.json'
paths = sorted(set(tracked + paths) - {str(output.relative_to(CHECKOUT))})
files = {}
for name in paths:
    path = CHECKOUT / name
    raw = path.read_bytes()
    text = raw.decode()
    assert raw.endswith(b'\n') and not raw.endswith(b'\n\n'), name
    assert all(line.rstrip() == line for line in text.splitlines()), name
    assert len(text.splitlines()) <= 250, name
    if path.suffix == '.py':
        ast.parse(text)
    if path.suffix == '.json':
        json.loads(text)
    files[name] = dict(lines=len(text.splitlines()), sha256=digest(path))
summary = json.loads((CACHE / 'summary.json').read_text())
assert summary['all_pass'] and len(summary['gates']) == 6
assert all(row['passes'] and row['pairs'] == 20 and row['child_returncode'] == 0
           and row['minimum_batch_seconds'] > 1 and row['paired_95pct_ci'][1] < 3
           for row in summary['gates'].values())
assert json.loads((CACHE / 'before-public-metrics.json').read_text()) == \
    json.loads((CACHE / 'after-public-metrics.json').read_text())
cli = [CACHE / (stage + '-' + label + '-cli.log')
       for stage in ('before', 'after') for label in ('candidate', 'current', 'frozen')]
assert len({digest(path) for path in cli}) == 1
assert digest(CACHE / 'qualification-verify.py') == digest(HERE / 'qualification-verify.py')
post = json.loads((HERE / 'root-postcheck.json').read_text())
assert post['all_six_measurements_exit_zero'] and post['qualification']
payload = dict(time=now(), files=files, file_count=len(files), maximum_lines=max(
    row['lines'] for row in files.values()), all_six_qualified=True,
    cli_sha256=digest(cli[0]), public_before_after_equal=True,
    frozen_inputs_unchanged=True, integrity_returncode=0)
write_json(output, payload)
print(json.dumps(dict(files=len(files), maximum_lines=payload['maximum_lines'],
                      ast_json_whitespace_eof=True, all_six_qualified=True,
                      integrity_returncode=0)))
