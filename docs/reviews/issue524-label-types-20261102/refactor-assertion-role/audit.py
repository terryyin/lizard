"""Check retained proof correspondence without executing parser or test work."""
from collections import Counter
import hashlib
import json
from pathlib import Path


CHECKOUT = Path(__file__).resolve().parents[4]
REVIEW = CHECKOUT / 'docs/reviews/issue524-label-types-20261102'
INCOMING = REVIEW / 'implementation-assertion-role'
OUT = Path(__file__).resolve().parent


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def read(path):
    return json.loads(path.read_text())


manifest_path = INCOMING / 'manifest.json'
expected_manifest = 'c5640bfc84aed773154e0a74b1e8ed4d7e94b1a9bb47721bc6c34d3da55c3ae3'
assert sha(manifest_path) == expected_manifest
manifest = read(manifest_path)
runtime = manifest['after_runtime_hashes']
tests = manifest['after_test_hashes']
assert len(runtime) == 108
assert len(tests) == 157
for name, expected in {**runtime, **tests}.items():
    assert sha(CHECKOUT / name) == expected, name

stories = []
for entry in manifest['story_files']:
    path = CHECKOUT / entry['path']
    actual = sha(path)
    assert actual == entry['after_sha256'], entry['path']
    lines = len(path.read_text().splitlines())
    assert lines == entry['lines']
    assert lines <= 250
    stories.append({'path': entry['path'], 'sha256': actual, 'lines': lines})
assert len(stories) == 11

proofs = []
for name, result in (
        ('languages-after-export.json', '1387 passed, 5 skipped'),
        ('consumers-after-export.json', '989 passed, 2 skipped'),
        ('outputs-after-export.json', '64 passed')):
    path = INCOMING / name
    proof = read(path)
    assert proof['exit_code'] == 0
    assert result in proof['stdout']
    for name_, expected in proof['hashes'].items():
        assert sha(CHECKOUT / name_) == expected, name_
    proofs.append({
        'path': str(path.relative_to(REVIEW)), 'sha256': sha(path),
        'command': proof['command'], 'exit_code': proof['exit_code'],
        'terminal_result': proof['stdout'].strip().splitlines()[-1],
        'boundary': 'reused; no implementation/setup/assertion edit by refactor',
        'inspected_hashes_match': True,
    })

fixture_path = INCOMING / 'cases-after-export.json'
cases = read(fixture_path)
previous = read(INCOMING / 'cases.json')
assert len(cases) == 490
assert len(previous) == 476
canon = lambda row: json.dumps(row, sort_keys=True, separators=(',', ':'))
assert not (Counter(map(canon, previous)) - Counter(map(canon, cases)))
capture = read(INCOMING / 'fixture-capture-after-export.json')
assert capture['exit_code'] == 0
assert capture['case_count'] == 490
assert '490 passed' in capture['stdout']
fixture_correspondence = read(INCOMING / 'fixture-correspondence-after-export.json')
assert fixture_correspondence['all_previous200_sources_wrappers_records_unchanged']
assert fixture_correspondence['all476_sources_wrappers_records_unchanged']
calls_path = INCOMING / 'calls-correspondence-after-export.json'
calls = read(calls_path)
assert calls['exit_code'] == 0
assert calls['same_frozen40_hashes']
assert calls['same80_default_NS_records']
assert calls['same_dispatch_counts']
assert calls['timing'] is False

consumer_path = REVIEW / 'refactor-final/consumer-dispositions.json'
consumers = read(consumer_path)
counts = dict(Counter(row['disposition'] for row in consumers))
assert len(consumers) == 719
assert counts == {'excluded': 547, 'run': 59, 'reused': 113}
assert all(row.get('purpose') for row in consumers)
edges_path = INCOMING / 'source-edges-after-export.json'
edges = read(edges_path)
assert len(edges) == 6

result = {
    'outcome': 'none — already clean',
    'refactor_product_or_test_edits': [],
    'runtime_manifest_sha256': expected_manifest,
    'runtime_files_match_live_bytes': len(runtime),
    'test_files_match_live_bytes': len(tests),
    'story_files': stories,
    'accepted_proof': proofs,
    'fixture': {
        'path': str(fixture_path.relative_to(REVIEW)), 'sha256': sha(fixture_path),
        'count': 490, 'all_prior476_actual_records_identical': True,
        'prior200_correspondence_receipt':
            'implementation-assertion-role/fixture-correspondence-after-export.json',
        'capture_command': capture['command'], 'capture_exit_code': 0,
        'capture_terminal_result': capture['stdout'].strip().splitlines()[-1],
    },
    'calls': {
        'path': str(calls_path.relative_to(REVIEW)), 'sha256': sha(calls_path),
        'command': calls['command'], 'exit_code': 0,
        'same_frozen40_sources_80_default_NS_records_dispatch_counts': True,
        'timing': False,
    },
    'consumer_trace': {
        'generated_by': 'installed consumer-trace.mjs; retained aggregate/import traces',
        'record': str(consumer_path.relative_to(REVIEW)),
        'sha256': sha(consumer_path), 'count': len(consumers),
        'accepted_dispositions': counts,
        'review_disposition': 'reused with current matching focused proof',
        'directed_edges': str(edges_path.relative_to(REVIEW)),
        'directed_edges_sha256': sha(edges_path), 'directed_edge_count': len(edges),
        'uncovered_consumer_gaps': [],
    },
    'verification': 'skipped — no refactor edits',
    'expensive_proof': 'not launched; root owns current full checks and requalification',
    'required_deferred_owner': 'root',
    'required_deferred': [
        'full pytest/make and applicable style/hook checks',
        'freeze expanded490 fixture corpus and serial strict below3% qualification',
        'current trunk/exact head hosted checks, publication, merge and issue closure',
    ],
    'no_owned_running_processes': True,
}
(OUT / 'correspondence.json').write_text(json.dumps(result, indent=2) + '\n')
print(json.dumps({
    'runtime_matches': len(runtime), 'test_matches': len(tests),
    'story_files': len(stories), 'fixtures': len(cases),
    'prior476_actual_records_identical': True, 'consumer_dispositions': counts,
    'directed_edges': len(edges), 'no_product_or_test_edits': True,
    'test_commands_launched': 0, 'timing_commands_launched': 0,
}, indent=2))
