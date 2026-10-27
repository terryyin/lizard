"""Bounded attribution using the immutable API worker's analysis loop shape."""
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys

FROZEN = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-final-ec2ec815-setup2')
OUT = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-ec2-attribution')
DOCS = Path('/Users/terryyin/git/lizard/.worktrees/preserve-arrow-boundaries-across-ternary-and-typ/docs/reviews/fabio-malpezzi-20261027/pr534')
FIELDS = ('name', 'start_line', 'end_line', 'cyclomatic_complexity',
          'nloc', 'token_count', 'parameter_count')

if '--worker' not in sys.argv:
    decision = dict(question='Attribute exact current/candidate API overhead; successful versus false probes',
                    selection='100 corpus passes for each current/candidate and original128/augmented166',
                    shape='Analyze only inside profile; public fields observed outside it',
                    resources='One active core serially, modest memory; no other owned heavy work',
                    conditions='16 CPUs, load8.72/7.99/7.71, about13.6GiB inactive; native VM unknown',
                    reconsider='Before repeats or changed contention; no throughput acceptance or timing retry')
    (OUT / 'decision.json').write_text(json.dumps(decision, indent=2) + '\n')
    for corpus in ('original', 'augmented'):
        for label in ('current', 'candidate'):
            command = [sys.executable, __file__, '--worker', corpus, label]
            completed = subprocess.run(command, cwd='/private/tmp', capture_output=True, text=True)
            name = corpus + '-' + label
            (OUT / (name + '.log')).write_text(completed.stdout + completed.stderr)
            receipt = dict(command=command, returncode=completed.returncode)
            (OUT / (name + '-receipt.json')).write_text(json.dumps(receipt) + '\n')
            print(json.dumps(dict(profile=name, returncode=completed.returncode)), flush=True)
            if completed.returncode:
                print(completed.stdout + completed.stderr)
                raise SystemExit(completed.returncode)
    raise SystemExit(0)

import cProfile
import importlib
import inspect
import profile
import pstats
import time

corpus, label = sys.argv[2:]
snapshot = FROZEN / label
assert not Path(profile.__file__).resolve().is_relative_to(snapshot)
sys.path.insert(0, str(snapshot))
import lizard

modules = ('lizard', 'lizard_core.file_analyzer', 'lizard_languages',
           'lizard_languages.typescript', 'lizard_languages.typescript_states',
           'lizard_languages.javascript_functions', 'test.test_languages.testES6')
imports = {name: importlib.import_module(name).__file__ for name in modules}
assert all(Path(path).resolve().is_relative_to(snapshot) for path in imports.values())
source_hashes = json.loads((DOCS / ('final-' + label + '-source-hashes.json')).read_text())

def check_sources():
    assert all(hashlib.sha256((snapshot / name).read_bytes()).hexdigest() == digest
               for name, digest in source_hashes.items())


check_sources()
files = sorted((FROZEN / corpus / 'corpus' / 'affected').glob('*'))
cases = [(str(path), path.read_text()) for path in files]
assert len(cases) == (128 if corpus == 'original' else 166)


def batch():
    for name, source in cases:
        lizard.analyze_file.analyze_source_code(name, source)


def metrics():
    return {name: [[getattr(function, field) for field in FIELDS]
                   for function in lizard.analyze_file.analyze_source_code(name, source).function_list]
            for name, source in cases}


before = metrics()
observations = dict(probes_true=0, probes_false=0, annotation_foreground_calls=0,
                    annotation_probe_calls=0, probes=[])
active = {}


def observe(frame, event, result):
    name = frame.f_code.co_name
    if name == 'return_type_colon':
        if event == 'call':
            active[id(frame)] = time.perf_counter()
        elif event == 'return':
            observations['probes_true' if result else 'probes_false'] += 1
            observations['probes'].append(dict(result=bool(result),
                observed_seconds=time.perf_counter() - active.pop(id(frame))))
    elif event == 'call' and name == '__call__':
        owner = frame.f_locals.get('self')
        if type(owner).__name__ == 'TypeScriptTypeAnnotationStates':
            key = 'annotation_probe_calls' if owner.context is None else 'annotation_foreground_calls'
            observations[key] += 1


sys.setprofile(observe)
batch()
sys.setprofile(None)
assert not active
assert metrics() == before
for _ in range(5):
    batch()
profiler = cProfile.Profile()
profiler.enable()
for _ in range(100):
    batch()
profiler.disable()
assert metrics() == before
check_sources()
assert all(Path(path).resolve().is_relative_to(snapshot) for path in imports.values())
stats = pstats.Stats(profiler)
rows = []
by_file = {}
for (file, line, name), values in stats.stats.items():
    calls, total_calls, own, cumulative, _ = values
    rows.append(dict(file=file, line=line, function=name, calls=calls,
                     total_calls=total_calls, own_seconds=own, cumulative_seconds=cumulative))
    by_file[file] = by_file.get(file, 0) + own
rows.sort(key=lambda row: row['own_seconds'], reverse=True)
destination = OUT / (corpus + '-' + label + '.json')
assert not destination.exists()
profiler.dump_stats(str(destination.with_suffix('.pstats')))
payload = dict(snapshot=str(snapshot), corpus=corpus, cases=len(cases), loops=100,
               imports=imports, metrics=before, unchanged_public_fields=True,
               observations=observations, total_seconds=stats.total_tt,
               calls=stats.total_calls, own_by_file=by_file, rows=rows)
destination.write_text(json.dumps(payload, indent=2) + '\n')
print(json.dumps(dict(profile=corpus + '-' + label, total_seconds=stats.total_tt,
                     counts={key: value for key, value in observations.items() if key != 'probes'},
                     top=rows[:15])), flush=True)
