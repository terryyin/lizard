"""Record the bounded export-default return and immutable prior correspondence."""
from collections import Counter
import difflib
import hashlib
import json
import os
from pathlib import Path
import platform
import subprocess
import sys


root = Path.cwd().resolve()
out = Path(__file__).resolve().parent
evidence = out.parent / 'evidence'


def read(path):
    return json.loads(path.read_text())


def write(name, data):
    (out / name).write_text(json.dumps(data, indent=2) + '\n')


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def run(script, arguments):
    args = [sys.executable, '-I', str(script)] + arguments
    result = subprocess.run(args, capture_output=True, text=True)
    receipt = {'command': 'PIP_QUIET=3 nix develop -c python -I '
               + ' '.join([str(script)] + arguments),
               'exit_code': result.returncode,
               'stdout': result.stdout, 'stderr': result.stderr}
    assert result.returncode == 0, receipt
    return receipt


before = read(out / 'manifest-before-export.json')
export_receipt = run(evidence / 'export_default_probe.py', [
    '--checkout', str(root), '--out', str(out / 'export-default-after.json')])
baseline = read(evidence / 'export-default-baseline.json')['observations']
prior = read(evidence / 'export-default-candidate.json')['observations']
current = read(out / 'export-default-after.json')['observations']
assert len(current) == len(baseline) == len(prior) == 14
assert current == baseline
assert sum(a != b for a, b in zip(prior, baseline)) == 14
export_receipt.update(rows=14, all14_exact_baseline=True,
                      all14_prior_candidate_regressions=True)
write('export-provenance.json', export_receipt)

calls_receipt = run(out.parent / 'implementation-cost/observe_calls.py', [
    '--checkout', str(root), '--corpus',
    '/Users/terryyin/.cache/issue524-label-types-20261102/calls',
    '--out', str(out / 'calls-after-export.json')])
calls_before, calls = read(out / 'calls-after.json'), read(out / 'calls-after-export.json')
assert calls_before['corpus'] == calls['corpus']
assert calls_before['observations'] == calls['observations']
assert calls_before['dispatch_counts'] == calls['dispatch_counts']
calls_receipt.update(same_frozen40_hashes=True, same80_default_NS_records=True,
                      same_dispatch_counts=True, timing=False)
write('calls-correspondence-after-export.json', calls_receipt)

old_cases, cases = read(out / 'cases.json'), read(out / 'cases-after-export.json')
key = lambda row: json.dumps(row, sort_keys=True)
old_count, count = Counter(map(key, old_cases)), Counter(map(key, cases))
assert len(old_cases) == 476 and len(cases) == 490
assert not old_count - count
added = [json.loads(row) for row in (count - old_count).elements()]
assert len(added) == 14 and all(r['source'].startswith('export default ') for r in added)
assert cases[:200] == read(out / 'before-cases.json')
write('fixture-correspondence-after-export.json', {
    'before_file': 'cases.json', 'before_count': 476,
    'after_file': 'cases-after-export.json', 'after_count': 490,
    'all476_sources_wrappers_records_unchanged': True,
    'all_previous200_sources_wrappers_records_unchanged': True,
    'added_count': 14, 'added_rows': added})

declarations = 'lizard_languages/typescript_declarations.py'
test = 'test/test_languages/testTypeScriptAssertionRoles.py'
old_declarations = (out / 'typescript_declarations-before-export.py').read_text()
assert hashlib.sha256(old_declarations.encode()).hexdigest() == before['after_runtime_hashes'][declarations]
test_text = (root / test).read_text()
row = " ('export-default', 'export default CALL;\\n', 1, None),\n"
assert test_text.count(row) == 1
old_test = test_text.replace(row, '')
assert hashlib.sha256(old_test.encode()).hexdigest() == before['after_test_hashes'][test]
(out / 'testTypeScriptAssertionRoles-before-export.py').write_text(old_test)
patch = ''
for name, old in [(declarations, old_declarations), (test, old_test)]:
    patch += ''.join(difflib.unified_diff(old.splitlines(True),
                     (root / name).read_text().splitlines(True),
                     fromfile='before-export/' + name, tofile='after-export/' + name))
(out / 'export.patch').write_text(patch)
full_patch = ''
for name in before['runtime_changed'] + [test]:
    old = '' if name == test else (out / 'before' / name).read_text()
    full_patch += ''.join(difflib.unified_diff(old.splitlines(True),
                         (root / name).read_text().splitlines(True),
                         fromfile='candidate2/' + name, tofile='after-export/' + name))
(out / 'correction-after-export.patch').write_text(full_patch)

manifest = read(out / 'manifest.json')
manifest['after_runtime_hashes'] = {p: digest(root / p) for p in before['after_runtime_hashes']}
manifest['after_test_hashes'] = {p: digest(root / p) for p in before['after_test_hashes']}
assert [p for p in manifest['after_runtime_hashes'] if manifest['after_runtime_hashes'][p]
        != before['after_runtime_hashes'][p]] == [declarations]
assert [p for p in manifest['after_test_hashes'] if manifest['after_test_hashes'][p]
        != before['after_test_hashes'][p]] == [test]
for item in manifest['story_files']:
    path = root / item['path']
    item.update(after_sha256=digest(path), lines=len(path.read_text().splitlines()))
    assert item['lines'] <= 250
manifest.update(fixtures=490, fixture_file='cases-after-export.json',
                fixture_receipt='fixture-capture-after-export.json',
                before_export_manifest='manifest-before-export.json',
                all_previous476_fixture_sources_wrappers_records_unchanged=True,
                export_default_observations=14,
                export_patch_sha256=digest(out / 'export.patch'),
                correction_after_export_patch_sha256=digest(out / 'correction-after-export.patch'),
                proof=['languages-after-export.json', 'consumers-after-export.json',
                       'outputs-after-export.json', 'fixture-capture-after-export.json',
                       'export-provenance.json', 'calls-correspondence-after-export.json'],
                known_directed_gaps='All observed G14-G17 handoffs have passing regressions and controls; no confirmed loss deferred.',
                no_owned_running_commands=True)
manifest['runtime_provenance'].update(host=platform.node(), cpu_count=os.cpu_count(),
                                      load=list(os.getloadavg()))
write('manifest.json', manifest)

edges = read(out / 'source-edges.json')
for edge in edges:
    edge['proof'] = [p.replace('languages-final.json', 'languages-after-export.json')
                     .replace('consumers-final.json', 'consumers-after-export.json')
                     .replace('outputs-final.json', 'outputs-after-export.json')
                     .replace('fixture-capture.json', 'fixture-capture-after-export.json')
                     .replace('calls-correspondence.json', 'calls-correspondence-after-export.json')
                     for p in edge['proof']]
edges[0]['proof'] += ['export-red.json', 'export-green.json', 'export-provenance.json']
edges[0]['observed'] += ' Export-default adds 14 runtime-call cases: exact root observations now equal baseline; 490 captured cases preserve every prior 476 record.'
edges[0]['setup'] += ' The local assertion-prefix set additionally owns default; regex and ASI sets remain read-only and unchanged.'
edges[-1]['observed'] = 'All prior476 fixture sources/wrappers/records identical;490 actual captures,1387 language,989 consumer,64 output checks pass. Frozen40 default/NS80records and dispatch counts unchanged; no timing verdict.'
write('source-edges-after-export.json', edges)
consumers = read(out / 'consumer-correspondence.json')
consumers.update(current_proof=manifest['proof'] + ['keyword-owner-after.json',
                 'reference-candidate2.json', 'reference-after.json'],
                 directed_edges='source-edges-after-export.json',
                 export_return='Only local default prefix membership and the existing CALL_CONTEXTS row changed; remaining accepted719 dispositions and476 boundary observations are reused with current focused suites.')
write('consumer-correspondence-after-export.json', consumers)
write('final-provenance-after-export.json', {
    'command': 'PIP_QUIET=3 nix develop -c python ' + str(Path(__file__).resolve()),
    'exit_code': 0, 'export_rows': 14, 'fixtures': 490,
    'all108_runtime_hashes_recorded': len(manifest['after_runtime_hashes']) == 108,
    'runtime_files_changed_since476': [declarations],
    'test_files_changed_since476': [test],
    'manifest_sha256': digest(out / 'manifest.json'),
    'export_patch_sha256': digest(out / 'export.patch'),
    'timing': False, 'no_owned_subprocesses_remaining': True})
print(json.dumps({'terminal': 0, 'fixtures':490, 'export_rows':14,
                  'manifest_sha256':digest(out / 'manifest.json'),
                  'changed': [i for i in manifest['story_files'] if i['path'] in (declarations, test)],
                  'calls80_and_dispatch_counts_unchanged':True}))
