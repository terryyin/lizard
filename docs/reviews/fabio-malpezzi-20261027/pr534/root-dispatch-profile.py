"""One after-change diagnostic using the unchanged affected API loop shape."""
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys

CHECKOUT = Path('/Users/terryyin/git/lizard/.worktrees/preserve-arrow-boundaries-across-ternary-and-typ')
FROZEN = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-final-ec2ec815-setup2')
BASELINE = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-reader-dispatch')
CACHE = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-root-dispatch')
DOCS = CHECKOUT / 'docs/reviews/fabio-malpezzi-20261027/pr534'
FIELDS = ('name', 'start_line', 'end_line', 'cyclomatic_complexity',
          'nloc', 'token_count', 'parameter_count')

if '--worker' not in sys.argv:
    cpus = int(subprocess.check_output(['sysctl', '-n', 'hw.ncpu']))
    load = os.getloadavg()
    assert load[0] < cpus, (load, cpus)
    decision = dict(host=os.uname().nodename, cpus=cpus, load=load,
                    vm=subprocess.check_output(['vm_stat'], text=True),
                    selection='Two serial working profiles, 100 passes for original128/augmented166',
                    shape='Public analysis only inside profile, field extraction before/after',
                    ownership='No other owned heavy work; native VM activity remains unknown',
                    decision='One active core and modest memory; reuse exact13c baseline profiles',
                    reconsider='Changed contention/failure before any repeat; no throughput claim')
    (CACHE / 'profile-decision.json').write_text(json.dumps(decision, indent=2) + '\n')
    temporary = Path('/private/tmp/lizard-seed015-root-dispatch-profile.py')
    shutil.copyfile(__file__, temporary)
    for corpus in ('original', 'augmented'):
        command = [sys.executable, str(temporary), '--worker', corpus]
        result = subprocess.run(command, cwd='/private/tmp', capture_output=True, text=True)
        (CACHE / (corpus + '-profile.log')).write_text(result.stdout + result.stderr)
        (CACHE / (corpus + '-profile-receipt.json')).write_text(
            json.dumps(dict(command=command, returncode=result.returncode)) + '\n')
        print(json.dumps(dict(profile=corpus, returncode=result.returncode)), flush=True)
        if result.returncode:
            print(result.stdout + result.stderr)
            raise SystemExit(result.returncode)
    raise SystemExit(0)

import cProfile
import importlib
import profile
import pstats

corpus = sys.argv[2]
assert not Path(profile.__file__).resolve().is_relative_to(CHECKOUT)
sys.path.insert(0, str(CHECKOUT))
import lizard

modules = ('lizard', 'lizard_core.file_analyzer', 'lizard_languages',
           'lizard_languages.typescript', 'lizard_languages.typescript_states',
           'lizard_languages.javascript_functions', 'test.test_languages.testES6')
imports = {name: importlib.import_module(name).__file__ for name in modules}
assert all(Path(path).resolve().is_relative_to(CHECKOUT) for path in imports.values())
proof = json.loads((DOCS / 'root-dispatch-proof.json').read_text())
hashes = proof['working_product_sha256']


def check_sources():
    assert all(hashlib.sha256((CHECKOUT / name).read_bytes()).hexdigest() == value
               for name, value in hashes.items())


check_sources()
files = sorted((FROZEN / corpus / 'corpus/affected').glob('*'))
cases = [(str(path), path.read_text()) for path in files]
baseline = json.loads((BASELINE / (corpus + '-working-profile.json')).read_text())
reference = json.loads((DOCS / 'dispatch-candidate-source-hashes.json').read_text())
assert baseline['product_sha256'] == reference
other = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-dispatch-13c633b8')
other = other / corpus / 'corpus/affected'
assert {path.name for path in files} == {path.name for path in other.glob('*')}
assert all(path.read_bytes() == (other / path.name).read_bytes() for path in files)
assert len(cases) == (128 if corpus == 'original' else 166)


def batch():
    for name, source in cases:
        lizard.analyze_file.analyze_source_code(name, source)


def metrics():
    return {name: [[getattr(function, field) for field in FIELDS]
                   for function in lizard.analyze_file.analyze_source_code(name, source).function_list]
            for name, source in cases}


before = metrics()
assert before == baseline['metrics']
for _ in range(5):
    batch()
profiler = cProfile.Profile()
profiler.enable()
for _ in range(100):
    batch()
profiler.disable()
assert metrics() == before
check_sources()
stats = pstats.Stats(profiler)
rows = []
for (file, line, name), values in stats.stats.items():
    calls, total_calls, own, cumulative, _ = values
    rows.append(dict(file=file, line=line, function=name, calls=calls,
                     total_calls=total_calls, own_seconds=own, cumulative_seconds=cumulative))
rows.sort(key=lambda row: row['own_seconds'], reverse=True)
dispatch = [row for row in rows if row['file'].endswith(('typescript.py', 'typescript_states.py', 'code_reader.py'))
            and row['function'] in ('__call__', 'process_token')]
assert not any(row['function'] == 'process_token' and row['file'].endswith('code_reader.py')
               for row in rows)
destination = CACHE / (corpus + '-working-profile.json')
assert not destination.exists()
profiler.dump_stats(str(destination.with_suffix('.pstats')))
payload = dict(working='uncommitted; not performance-qualified', loops=100, cases=len(cases),
               fields=FIELDS, metrics=before, imports=imports, unchanged_public_fields=True,
               product_sha256=hashes, total_seconds=stats.total_tt, rows=rows,
               dispatch=dispatch, baseline_seconds=baseline['total_seconds'])
destination.write_text(json.dumps(payload, indent=2) + '\n')
print(json.dumps(dict(corpus=corpus, total_seconds=stats.total_tt,
                     old_seconds=baseline['total_seconds'], dispatch=dispatch)), flush=True)
