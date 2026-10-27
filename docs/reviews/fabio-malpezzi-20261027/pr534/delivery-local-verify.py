"""Prove the resolved delivery tree still has the qualified public behavior."""
from datetime import datetime, timezone
import hashlib
import json
from pathlib import Path
import shutil
import subprocess
import sys

HERE = Path(__file__).resolve().parent
CHECKOUT = HERE.parents[3]
CACHE = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-delivery-68a1ce8')
QUALIFIED = CACHE.parent / 'seed015-root-737aeaf7'
REVISION = '737aeaf7334b8e64f6ab0011c0f2ea65c356b06d'
HEAD = '68a1ce8d4808a799b43818a0663fc53f646bb325'
CONTRIBUTOR = 'f4074ae860c7999cc37dd081a6f7ebfa777adc83'
CURRENT = '34cd43ddd1915438f3b440088672bfe4a08d96bf'


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def git(*args):
    return subprocess.check_output(['git'] + list(args), cwd=CHECKOUT)


def write_json(path, value):
    path.write_text(json.dumps(value, separators=(',', ':')) + '\n')


def run_logged(label, args, cwd, expected=0):
    log = CACHE / (label + '.log')
    assert not log.exists(), 'Do not overwrite retained proof'
    result = subprocess.run(args, cwd=cwd, capture_output=True)
    log.write_bytes(result.stdout)
    stderr = CACHE / (label + '.stderr')
    stderr.write_bytes(result.stderr)
    receipt = dict(command=args, cwd=str(cwd), returncode=result.returncode,
                   expected_returncode=expected, stdout=str(log), stdout_sha256=digest(log),
                   stderr=str(stderr), stderr_sha256=digest(stderr))
    write_json(CACHE / (label + '-receipt.json'), receipt)
    assert result.returncode == expected and not result.stderr, receipt
    return receipt


assert git('rev-parse', 'HEAD').decode().strip() == HEAD
assert git('rev-parse', 'MERGE_HEAD').decode().strip() == CONTRIBUTOR
assert not git('diff', '--name-only', '--diff-filter=U')
paths = ('lizard.py', 'lizard_core', 'lizard_languages', 'lizard_ext', 'test')
assert not git('diff', '--name-only', REVISION, '--', *paths)
products = [CHECKOUT / 'lizard.py']
for directory in ('lizard_core', 'lizard_languages', 'lizard_ext'):
    products += sorted((CHECKOUT / directory).rglob('*.py'))
actual = {str(path.relative_to(CHECKOUT)): digest(path) for path in products}
expected = json.loads((HERE / 'root-candidate-source-hashes.json').read_text())
assert actual == expected and len(actual) == 104
assert all(digest(QUALIFIED / 'candidate' / name) == value for name, value in actual.items())
tests = git('ls-files', '-z', '--', 'test').decode().split('\0')
test_hashes = {name: digest(CHECKOUT / name) for name in tests if name}
write_json(CACHE / 'test-hashes.json', test_hashes)
es6 = 'test/test_languages/testES6.py'
assert (CHECKOUT / es6).read_bytes() == git('show', CURRENT + ':' + es6)
inputs = QUALIFIED / 'augmented-inputs.json'
freeze = json.loads((HERE / 'root-freeze.json').read_text())
assert digest(inputs) == freeze['augmented_inputs_sha256']
helper = CACHE / 'qualification-verify.py'
assert not helper.exists()
shutil.copyfile(HERE / 'qualification-verify.py', helper)
metrics = run_logged('public', [sys.executable, str(helper), str(CHECKOUT),
                     str(inputs), '--require-expected'], CACHE)
rows = [json.loads(line) for line in Path(metrics['stdout']).read_text().splitlines()]
assert rows[-1]['cases'] == 166 and not rows[-1]['failures']
observed = {row['case']: row['actual'] for row in rows if 'case' in row}
previous = json.loads((QUALIFIED / 'after-public-metrics.json').read_text())['candidate']
assert observed == previous and len(observed) == 166
sources = json.loads(inputs.read_text())
authored = [row for row in sources if row['label'] == 'authored']
assert len(authored) == 4 and all(row['expected'] is not None for row in authored)
assert all(observed[row['filename']] for row in authored)
targets = [str(QUALIFIED / 'original/corpus' / name) for name in (
    'lizard.py', 'lizard_core', 'lizard_languages', 'lizard_ext')]
cli = run_logged('cli', [sys.executable, str(CHECKOUT / 'lizard.py')] + targets,
                 QUALIFIED / 'original/corpus', expected=1)
assert Path(cli['stdout']).read_bytes() == (QUALIFIED / 'after-candidate-cli.log').read_bytes()
trace = json.loads((HERE / 'root-dispatch-proof.json').read_text())['trace']
assert digest(Path(trace['path'])) == trace['sha256']
trace_lines = Path(trace['path']).read_text().splitlines()
located = trace_lines[trace_lines.index('## Located consumers'):]
assert len([line for line in located if line.startswith('| `')]) == trace['rows'] == 492
assert not any(line.startswith('| `') and line.endswith('| |') for line in located)
shutil.copyfile(trace['path'], CACHE / 'consumers-reused.md')
web = run_logged('web-applicability', [sys.executable, str(CHECKOUT / 'scripts/web_changes.py'),
                 'pull_request', CURRENT, HEAD, '--default-branch', 'master'], CHECKOUT)
assert 'web=false' in Path(web['stdout']).read_text()
assert not git('diff', '--name-only', REVISION, '--', *paths)
assert actual == {name: digest(CHECKOUT / name) for name in actual}
proof = dict(time=datetime.now(timezone.utc).isoformat(), head=HEAD, merge_head=CONTRIBUTOR,
             current=CURRENT, qualified_revision=REVISION, python=sys.version,
             executable=sys.executable, actual_product_hashes=actual,
             runtime_and_all_tests_equal_qualified=True, test_files=len(test_hashes),
             test_hash_manifest=str(CACHE / 'test-hashes.json'),
             test_hash_manifest_sha256=digest(CACHE / 'test-hashes.json'),
             es6_target_byte_identical=True, es6_sha256=digest(CHECKOUT / es6),
             input_sha256=digest(inputs), cases=166, all_seven_fields_equal=True,
             authored_cases=[row['filename'] for row in authored],
             receipts=dict(public=metrics, cli=cli, web_applicability=web),
             reused_installed_trace=dict(**trace, current_copy=str(CACHE / 'consumers-reused.md'),
                 reason='Exact product/test contracts unchanged; final full suite supplements accepted492'),
             verification_returncode=0)
write_json(HERE / 'delivery-local-verification.json', proof)
print(json.dumps(dict(cases=166, product_hashes=104, all_seven_fields_equal=True,
                      cli_sha256=cli['stdout_sha256'], installed_trace_rows=492,
                      test_files=len(test_hashes), verification_returncode=0)), flush=True)
