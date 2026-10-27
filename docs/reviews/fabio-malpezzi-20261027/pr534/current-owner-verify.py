"""Observe the current-owner merge without changing frozen expectations."""

import hashlib
import io
import json
from pathlib import Path
import subprocess
import sys
import tarfile

HERE = Path(__file__).resolve().parent
CHECKOUT = HERE.parents[3]
CACHE = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-current-1bc7cbd')
INPUTS = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-optimized-8dac7b80/inputs.json')
REVISIONS = dict(before='3a1ed8ae69314f070118ffc05d317149db7eecf8',
                 current='1bc7cbd64bef352300195c66888139bdf8d8d8e0')
FIELDS = ('name', 'start_line', 'end_line', 'cyclomatic_complexity',
          'nloc', 'token_count', 'parameter_count')


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


if '--probe' in sys.argv:
    snapshot = Path(sys.argv[-1]).resolve()
    sys.path.insert(0, str(snapshot))
    import lizard
    assert Path(lizard.__file__).resolve().is_relative_to(snapshot)
    rows = []
    for extension in ('js', 'ts', 'jsx', 'tsx'):
        for alternate in ('y => y || d', '(y) => y || d'):
            source = ('function outer(c, d) {\n'
                      '  const f = c ? (x = (z => ({a: z}))(d)) : '
                      + alternate + ';\n  return 1;\n}\n'
                      'function after() { return 1; }\n')
            functions = lizard.analyze_file.analyze_source_code(
                'a.' + extension, source).function_list
            rows.append(dict(extension=extension, alternate=alternate,
                             source=source,
                             functions=[[getattr(f, field) for field in FIELDS]
                                        for f in functions],
                             parameters=[f.parameters for f in functions]))
    print(json.dumps(dict(snapshot=str(snapshot), imports=lizard.__file__, rows=rows)))
    raise SystemExit(0)


def run(command, output, expected=0):
    with output.open('w') as stream:
        result = subprocess.run(command, cwd='/private/tmp', stdout=stream,
                                stderr=subprocess.STDOUT)
    assert result.returncode == expected, (command, result.returncode, output)
    return dict(command=command, exit=result.returncode, log=str(output), sha256=digest(output))


CACHE.mkdir(exist_ok=True)
roots = dict(merged=CHECKOUT)
for name, revision in REVISIONS.items():
    root = CACHE / name
    if not root.exists():
        archive = subprocess.check_output(['git', 'archive', revision], cwd=CHECKOUT)
        root.mkdir()
        with tarfile.open(fileobj=io.BytesIO(archive)) as source:
            source.extractall(root)
    roots[name] = root
receipts = {}
for name, root in roots.items():
    command = [sys.executable, str(HERE / 'qualification-verify.py'), str(root), str(INPUTS)]
    if name != 'current':
        command.append('--require-expected')
    receipts[name + '_replay'] = run(command, CACHE / (name + '-metrics.jsonl'))
    command = [sys.executable, str(__file__), '--probe', str(root)]
    receipts[name + '_probe'] = run(command, CACHE / (name + '-probe.json'))
payload = dict(revisions=REVISIONS, merged_head=subprocess.check_output(
    ['git', 'rev-parse', 'HEAD'], cwd=CHECKOUT, text=True).strip(),
    fields=FIELDS, inputs_sha256=digest(INPUTS), receipts=receipts)
(CACHE / 'replay-receipts.json').write_text(json.dumps(payload, indent=2) + '\n')


def metrics(name):
    rows = [json.loads(line) for line in (CACHE / (name + '-metrics.jsonl')).read_text().splitlines()]
    return {row['case']: row['actual'] for row in rows if 'case' in row}


before, merged = metrics('before'), metrics('merged')
assert len(before) == len(merged) == 128
differences = []
for case, old in before.items():
    new = merged[case]
    assert len(old) == len(new), case
    for index, (old_function, new_function) in enumerate(zip(old, new)):
        for field, old_value, new_value in zip(FIELDS, old_function, new_function):
            if old_value != new_value:
                assert field == 'parameter_count', (case, field)
                differences.append(dict(case=case, function=index, field=field,
                                        before=old_value, after=new_value))
current_probe = json.loads((CACHE / 'current-probe.json').read_text())
merged_probe = json.loads((CACHE / 'merged-probe.json').read_text())
assert current_probe['rows'] == merged_probe['rows']
assert len(current_probe['rows']) == 8
for row in merged_probe['rows']:
    assert [f[:4] for f in row['functions']] == [
        ['(anonymous)', 2, 2, 1], ['(anonymous)', 2, 2, 2],
        ['outer', 1, 4, 2], ['after', 5, 5, 1]]

corpus = INPUTS.parent / 'corpus'
targets = [str(corpus / name) for name in ('lizard.py', 'lizard_core', 'lizard_languages', 'lizard_ext')]
cli = {}
for name, root in roots.items():
    command = [sys.executable, str(root / 'lizard.py')] + targets
    result = subprocess.run(command, cwd=corpus, capture_output=True)
    assert result.returncode == 1, (name, result.returncode, result.stderr)
    output = CACHE / (name + '-cli.txt')
    output.write_bytes(result.stdout)
    cli[name] = dict(command=command, exit=result.returncode,
                     stdout_sha256=digest(output), stderr=result.stderr.decode())
assert len({v['stdout_sha256'] for v in cli.values()}) == 1
payload.update(cases=128, expected_public_rows=True,
               unchanged_fields=FIELDS[:-1], parameter_count_changes=len(differences),
               runtime_initializer_journeys_equal_current=8, cli=cli)
(HERE / 'current-owner-metric-differences.json').write_text(
    '[\n' + ',\n'.join(json.dumps(row, separators=(',', ':')) for row in differences) + '\n]\n')
(HERE / 'current-owner-proof.json').write_text(json.dumps(payload, indent=2) + '\n')
print(json.dumps(dict(terminal_exits={k: v['exit'] for k, v in receipts.items()},
                     inputs_sha256=payload['inputs_sha256'], differences=len(differences),
                     cli_sha256={name: value['stdout_sha256'] for name, value in cli.items()})), flush=True)
