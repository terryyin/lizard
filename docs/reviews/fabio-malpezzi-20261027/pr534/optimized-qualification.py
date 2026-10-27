"""Freeze and measure the committed parser-boundary optimization once."""

import hashlib
import io
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tarfile

HERE = Path(__file__).resolve().parent
CHECKOUT = HERE.parents[3]
INITIAL = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-performance')
CACHE = INITIAL.parent / 'seed015-optimized-8dac7b80'
REVISIONS = {
    'candidate': '8dac7b80c2478d65b54d8ec96d6aa36bf4530b91',
    'current': '9979f106a955aef2460ed6fe7ebb5782b755bcad',
    'frozen': '2e21e404c1ab9f4e0b49acc80816e816629d0670',
}


def digest(data):
    return hashlib.sha256(data).hexdigest()


def command(*args):
    return subprocess.check_output(args, cwd=CHECKOUT, text=True).strip()


def compact_map(path, payload):
    path.write_text('{\n' + ',\n'.join(
        json.dumps(key) + ': ' + json.dumps(value)
        for key, value in payload.items()) + '\n}\n')


def source_hashes(snapshot):
    files = [snapshot / 'lizard.py']
    for directory in ('lizard_core', 'lizard_languages', 'lizard_ext'):
        files += sorted((snapshot / directory).rglob('*.py'))
    return {str(path.relative_to(snapshot)): digest(path.read_bytes()) for path in files}


def assert_inputs(freeze):
    assert digest((CACHE / 'paired.py').read_bytes()) == freeze['harness_sha256']
    assert digest((CACHE / 'manifest.json').read_bytes()) == freeze['original_manifest_sha256']
    recorded = set()
    for filename in ('affected-hashes.json', 'broad-hashes.json'):
        hashes = json.loads((CACHE / filename).read_text())
        recorded.update(hashes)
        assert all(digest((CACHE / 'corpus' / name).read_bytes()) == value
                   for name, value in hashes.items())
    assert recorded == {str(path.relative_to(CACHE / 'corpus'))
                        for path in (CACHE / 'corpus').rglob('*') if path.is_file()}
    for label in REVISIONS:
        assert source_hashes(CACHE / label) == json.loads(
            (CACHE / (label + '-source-hashes.json')).read_text())


def run_logged(args, filename):
    with (CACHE / filename).open('w') as output:
        result = subprocess.run(args, cwd=CACHE / 'corpus', stdout=output,
                                stderr=subprocess.STDOUT)
    result.check_returncode()


if '--measure' not in sys.argv:
    assert command('git', 'rev-parse', 'HEAD') == REVISIONS['candidate']
    assert command('git', 'rev-parse', 'origin/master') == REVISIONS['current']
    initial = json.loads((INITIAL / 'freeze.json').read_text())
    assert initial == json.loads((HERE / 'qualification-freeze.json').read_text())
    assert not CACHE.exists(), 'Do not overwrite any optimized experiment'
    CACHE.mkdir()
    shutil.copytree(INITIAL / 'corpus', CACHE / 'corpus')
    for filename in ('paired.py', 'manifest.json', 'inputs.json',
                     'affected-hashes.json', 'broad-hashes.json', 'addition-hashes.json'):
        shutil.copyfile(INITIAL / filename, CACHE / filename)
    assert digest((CACHE / 'paired.py').read_bytes()) == initial['harness_sha256']
    assert digest((CACHE / 'manifest.json').read_bytes()) == initial['original_manifest_sha256']
    for name in ('affected', 'broad', 'addition'):
        assert digest((CACHE / (name + '-hashes.json')).read_bytes()) == initial[name + '_manifest_sha256']
    assert len(json.loads((CACHE / 'affected-hashes.json').read_text())) == 128
    assert len(json.loads((CACHE / 'broad-hashes.json').read_text())) == 100
    implementations = {}
    for label, revision in REVISIONS.items():
        snapshot = CACHE / label
        snapshot.mkdir()
        archive = subprocess.check_output(['git', 'archive', revision], cwd=CHECKOUT)
        with tarfile.open(fileobj=io.BytesIO(archive)) as tree:
            tree.extractall(snapshot)
        implementations[label] = source_hashes(snapshot)
        compact_map(CACHE / (label + '-source-hashes.json'), implementations[label])
    freeze = dict(revisions=REVISIONS, harness_sha256=initial['harness_sha256'],
                  original_manifest_sha256=initial['original_manifest_sha256'],
                  driver_sha256=digest(Path(__file__).read_bytes()),
                  affected_files=128, broad_files=100,
                  affected_manifest_sha256=digest((CACHE / 'affected-hashes.json').read_bytes()),
                  broad_manifest_sha256=digest((CACHE / 'broad-hashes.json').read_bytes()),
                  addition_manifest_sha256=digest((CACHE / 'addition-hashes.json').read_bytes()),
                  source_hashes={label: digest(json.dumps(value, sort_keys=True).encode())
                                 for label, value in implementations.items()},
                  python=sys.version, executable=sys.executable)
    assert_inputs(freeze)
    (CACHE / 'freeze.json').write_text(json.dumps(freeze, indent=2) + '\n')
    shutil.copyfile(HERE / 'qualification-verify.py', CACHE / 'qualification-verify.py')
    cli_hashes = []
    for label in REVISIONS:
        run_logged([sys.executable, str(CACHE / 'qualification-verify.py'),
                    str(CACHE / label), str(CACHE / 'inputs.json')], label + '-metrics.jsonl')
        targets = [str(CACHE / 'corpus' / name) for name in (
            'lizard.py', 'lizard_core', 'lizard_languages', 'lizard_ext')]
        result = subprocess.run([sys.executable, str(CACHE / label / 'lizard.py')] + targets,
                                cwd=CACHE / 'corpus', stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        assert result.returncode == 1, (label, result.returncode, result.stderr)
        (CACHE / (label + '-cli-output.txt')).write_bytes(result.stdout)
        cli_hashes.append(digest(result.stdout))
        print(json.dumps(dict(proof=label, cli_exit=1, cli_sha256=cli_hashes[-1])), flush=True)
    assert len(set(cli_hashes)) == 1
    print(json.dumps(dict(frozen=freeze, cli_sha256=cli_hashes[0])), flush=True)
    raise SystemExit(0)

freeze = json.loads((CACHE / 'freeze.json').read_text())
assert freeze['revisions'] == REVISIONS
assert digest(Path(__file__).read_bytes()) == freeze['driver_sha256']
assert_inputs(freeze)
summary = json.loads((CACHE / 'candidate-metrics.jsonl').read_text().splitlines()[-1])
assert summary['cases'] == 128 and not summary['failures']
processes = subprocess.check_output(['ps', '-axo', 'pid,pcpu,pmem,command'], text=True).splitlines()
heavy = [line for line in processes if any(term in line for term in (
    'paired.py', 'python -m pytest', 'cProfile', 'qualification-profile.py'))]
decision = dict(host=command('hostname'), cpus=os.cpu_count(), load=os.getloadavg(),
                memory_bytes=command('sysctl', '-n', 'hw.memsize'), vm_stat=command('vm_stat'),
                competing_commands=heavy, owner='optimized_qualification',
                workload='four serial 20-pair warmed API/CLI comparisons, 1.5-second calibration',
                choice='run serially; modest memory, no other owned heavy work',
                unknowns='other native worktree activity may exist outside command observation',
                reconsider='changed contention, failures or unresolved noise before another attempt')
assert not heavy, heavy
assert not (CACHE / 'decision.json').exists(), 'Do not silently repeat a qualification'
(CACHE / 'decision.json').write_text(json.dumps(decision, indent=2) + '\n')
print(json.dumps(dict(prospective=decision)), flush=True)
for baseline in ('current', 'frozen'):
    for workload in ('api', 'cli'):
        label = baseline + '-' + workload
        destination = CACHE / (label + '.json')
        assert not destination.exists(), 'Do not overwrite an earlier measurement'
        args = [sys.executable, str(CACHE / 'paired.py'),
                '--baseline', str(CACHE / baseline), '--candidate', str(CACHE / 'candidate'),
                '--workload', workload, '--pairs', '20', '--seconds', '1.5',
                '--out', str(destination)]
        print(json.dumps(dict(start=label, command=args)), flush=True)
        run_logged(args, label + '-terminal.log')
        data = json.loads(destination.read_text())
        assert len(data['raw']) == 20
        assert min(value for pair in data['raw'] for value in pair.values()) > 1
        print(json.dumps(dict(finished=label, **{key: data[key] for key in (
            'medians', 'stdev', 'median_slowdown_pct', 'paired_geometric_slowdown_pct',
            'paired_95pct_ci', 'passes')})), flush=True)
assert_inputs(freeze)
