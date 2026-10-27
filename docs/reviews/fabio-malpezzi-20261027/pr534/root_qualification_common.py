"""Frozen inputs, receipts and public observations for exact qualification."""

from datetime import datetime, timezone
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys

HERE = Path(__file__).resolve().parent
CHECKOUT = HERE.parents[3]
HISTORICAL = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534')
INITIAL = HISTORICAL / 'seed015-dispatch-13c633b8'
CACHE = HISTORICAL / 'seed015-root-737aeaf7'
REVISIONS = {
    'candidate': '737aeaf7334b8e64f6ab0011c0f2ea65c356b06d',
    'current': '34cd43ddd1915438f3b440088672bfe4a08d96bf',
    'frozen': '2e21e404c1ab9f4e0b49acc80816e816629d0670',
}
HARNESS = '3064d8d77047db2f084ce9fd6ae09d98acf34453316b7df20fb184cbb3217bf5'
MANIFEST = '113ca1889836963fe6cfd7ba62c1aa90bd927eaedc4cd8cc7b46479ecb3bccd5'
CLI_HASH = '49e9a7922e48f71ac1702f2da38939d93c24b4339d0d3618dfdfdebe1287150b'
DRIVERS = ('root_qualification_common.py', 'root-qualification-freeze.py',
           'root-qualification-measure.py', 'root-qualification-postcheck.py')
GATES = (
    ('original-current-api', 'original', 'current', 'api'),
    ('original-current-cli', 'original', 'current', 'cli'),
    ('original-frozen-api', 'original', 'frozen', 'api'),
    ('original-frozen-cli', 'original', 'frozen', 'cli'),
    ('augmented-current-api', 'augmented', 'current', 'api'),
    ('augmented-frozen-api', 'augmented', 'frozen', 'api'),
)


def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def write_json(path, value):
    """Compact one record per map entry, keeping retained manifests readable."""
    path = Path(path)
    if isinstance(value, dict):
        body = '{\n' + ',\n'.join(json.dumps(k) + ': ' + json.dumps(v)
                                    for k, v in value.items()) + '\n}\n'
    else:
        body = '[\n' + ',\n'.join(json.dumps(v) for v in value) + '\n]\n'
    path.write_text(body)


def command(*args):
    return subprocess.check_output(args, cwd=CHECKOUT, text=True).strip()


def now():
    return datetime.now(timezone.utc).isoformat()


def source_hashes(snapshot):
    files = [snapshot / 'lizard.py']
    for directory in ('lizard_core', 'lizard_languages', 'lizard_ext'):
        files += sorted((snapshot / directory).rglob('*.py'))
    return {str(path.relative_to(snapshot)): digest(path) for path in files}


def corpus_hashes(root):
    return {str(path.relative_to(root)): digest(path)
            for path in sorted(root.rglob('*')) if path.is_file()}


def assert_inputs():
    freeze = json.loads((CACHE / 'freeze.json').read_text())
    assert freeze['revisions'] == REVISIONS
    assert freeze['gates'] == [list(gate) for gate in GATES]
    assert freeze['python'] == sys.version and freeze['executable'] == sys.executable
    assert all(digest(HERE / name) == value for name, value in freeze['drivers'].items())
    assert digest(CACHE / 'manifest.json') == MANIFEST
    for name in ('original', 'augmented'):
        runner = CACHE / name
        assert digest(runner / 'paired.py') == HARNESS
        assert digest(runner / 'manifest.json') == MANIFEST
        expected = json.loads((CACHE / (name + '-corpus-hashes.json')).read_text())
        assert corpus_hashes(runner / 'corpus') == expected
    for label in REVISIONS:
        expected = json.loads((CACHE / (label + '-source-hashes.json')).read_text())
        assert source_hashes(CACHE / label) == expected
    assert digest(CACHE / 'supplemental-inputs.json') == freeze['supplement_sha256']
    assert digest(CACHE / 'original-inputs.json') == freeze['original_inputs_sha256']
    assert digest(CACHE / 'augmented-inputs.json') == freeze['augmented_inputs_sha256']
    assert digest(CACHE / 'cli-reference.txt') == CLI_HASH
    return freeze


def run_logged(args, label, cwd=None, expected=0):
    log = CACHE / (label + '.log')
    receipt = CACHE / (label + '-receipt.json')
    assert not log.exists() and not receipt.exists(), label
    started = now()
    with log.open('w') as output:
        result = subprocess.run(args, cwd=cwd or CACHE, stdout=output,
                                stderr=subprocess.STDOUT)
    value = dict(command=args, cwd=str(cwd or CACHE), started=started, finished=now(),
                 returncode=result.returncode, expected_returncode=expected,
                 log=str(log), log_sha256=digest(log))
    write_json(receipt, value)
    assert result.returncode == expected, value
    return value


def observations(stage):
    verifier = CACHE / 'qualification-verify.py'
    reference = (CACHE / 'cli-reference.txt').read_bytes()
    old_prefix = str(HISTORICAL / 'seed015-optimized-8dac7b80/corpus').encode()
    new_prefix = str(CACHE / 'original/corpus').encode()
    expected_cli = reference.replace(old_prefix, new_prefix)
    receipts = {}
    metrics = {}
    cli_outputs = []
    for label in REVISIONS:
        args = [sys.executable, str(verifier), str(CACHE / label),
                str(CACHE / 'augmented-inputs.json')]
        receipt = run_logged(args, stage + '-' + label + '-metrics')
        lines = [json.loads(line) for line in Path(receipt['log']).read_text().splitlines()]
        assert lines[-1]['cases'] == 166
        if label == 'candidate':
            assert not lines[-1]['failures']
        metrics[label] = {row['case']: row['actual'] for row in lines if 'case' in row}
        receipts[label + '-metrics'] = receipt
        targets = [str(CACHE / 'original/corpus' / name) for name in (
            'lizard.py', 'lizard_core', 'lizard_languages', 'lizard_ext')]
        cli_args = [sys.executable, str(CACHE / label / 'lizard.py')] + targets
        cli = run_logged(cli_args, stage + '-' + label + '-cli', expected=1)
        actual_cli = Path(cli['log']).read_bytes()
        assert actual_cli == expected_cli, cli
        cli_outputs.append(actual_cli)
        receipts[label + '-cli'] = cli
    assert cli_outputs[0] == cli_outputs[1] == cli_outputs[2]
    accepted = HISTORICAL / 'seed015-root-dispatch/working-metrics.jsonl'
    old = [json.loads(line) for line in accepted.read_text().splitlines()]
    original = {row['case']: row['actual'] for row in old if 'case' in row}
    assert len(original) == 166
    assert all(metrics['candidate'][name] == fields for name, fields in original.items())
    candidate_hashes = source_hashes(CACHE / 'candidate')
    assert candidate_hashes == json.loads((HERE / 'root-dispatch-proof.json').read_text())['working_product_sha256']
    write_json(CACHE / (stage + '-public-metrics.json'), metrics)
    return receipts


def host_observation():
    processes = command('ps', '-axo', 'pid,pcpu,pmem,command').splitlines()
    heavy = [line for line in processes if any(term in line for term in (
        'paired.py', 'python -m pytest', 'qualification-profile.py', 'context-profile.py'))]
    return dict(time=now(), host=command('hostname'), cpus=os.cpu_count(),
                load=os.getloadavg(), memory_bytes=command('sysctl', '-n', 'hw.memsize'),
                vm_stat=command('vm_stat'), competing_commands=heavy,
                high_cpu=command('ps', '-arcwwxo', 'pid,pcpu,pmem,comm').splitlines()[:12])
