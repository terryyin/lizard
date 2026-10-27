"""Equal-output controls and canonical completed-binding correspondence."""
import hashlib
import json
from pathlib import Path
import subprocess
import sys

FROZEN = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-final-ec2ec815-setup2')
OUT = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-ec2-attribution')
FIELDS = ('name', 'start_line', 'end_line', 'cyclomatic_complexity',
          'nloc', 'token_count', 'parameter_count')
NAMES = ('block_branches.js', 'block_branches.ts', 'plain_ternary.js', 'internal_ternary.js')

if '--worker' not in sys.argv:
    for label in ('current', 'candidate'):
        command = [sys.executable, __file__, '--worker', label]
        result = subprocess.run(command, cwd='/private/tmp', capture_output=True, text=True)
        (OUT / ('control2-' + label + '.log')).write_text(result.stdout + result.stderr)
        (OUT / ('control2-' + label + '-receipt.json')).write_text(
            json.dumps(dict(command=command, returncode=result.returncode)) + '\n')
        print(json.dumps(dict(control=label, returncode=result.returncode)), flush=True)
        if result.returncode:
            print(result.stdout + result.stderr)
            raise SystemExit(result.returncode)
    current = json.loads((OUT / 'control2-current.json').read_text())
    candidate = json.loads((OUT / 'control2-candidate.json').read_text())
    assert current['metrics'] == candidate['metrics']
    print(json.dumps(dict(equal_output_controls=4, fields=7,
                         seconds={name: value['total_seconds'] for name, value in
                                  [('current', current), ('candidate', candidate)]},
                         binding_contexts=candidate['binding_contexts'])), flush=True)
    raise SystemExit(0)

import cProfile
import importlib
import profile
import pstats

label = sys.argv[2]
snapshot = FROZEN / label
sys.path.insert(0, str(snapshot))
import lizard

assert Path(lizard.__file__).resolve().is_relative_to(snapshot)
assert not Path(profile.__file__).resolve().is_relative_to(snapshot)
if label == 'candidate':
    from lizard_languages.javascript_functions import parameter_group

cases = [(name, (FROZEN / 'original/corpus/affected' / name).read_text()) for name in NAMES]


def batch():
    for name, source in cases:
        lizard.analyze_file.analyze_source_code(name, source)


def metrics():
    return {name: [[getattr(function, field) for field in FIELDS]
                   for function in lizard.analyze_file.analyze_source_code(name, source).function_list]
            for name, source in cases}


before = metrics()
binding_contexts = dict(accepted=0, without_pending=0, without_binding=0,
                        emptiness_mismatches=0, missing_origin=0, callback_origins=0,
                        eligibility_mismatches=0, rows=[])
origins = {}
calls = {}
filename = ''


def observe(frame, event, value):
    name = frame.f_code.co_name
    if name == '_read_binding_candidate':
        if event == 'call':
            calls[id(frame)] = dict(prefix=frame.f_locals['self']._prev_token,
                                   closing=frame.f_locals['closing'],
                                   callback=frame.f_locals.get('callback') is not None)
        elif event == 'return':
            origin = calls.pop(id(frame))
            if value is not None:
                origins[id(value)] = origin
    elif event == 'return' and name == 'return_annotation_context' and value is not None:
        state = frame.f_locals['state']
        group = frame.f_locals['group']
        binding = group._binding_candidate
        pending = state._pending_bindings
        origin = origins.get(id(group))
        binding_contexts['accepted'] += 1
        binding_contexts['without_pending'] += pending is None
        binding_contexts['without_binding'] += binding is None
        binding_contexts['missing_origin'] += origin is None
        binding_contexts['callback_origins'] += bool(origin and origin['callback'])
        empty = binding.at_start if binding else None
        binding_contexts['emptiness_mismatches'] += empty != group._empty_parenthesis
        eligibility = parameter_group(origin['prefix']) if origin else None
        binding_contexts['eligibility_mismatches'] += eligibility is not True
        binding_contexts['rows'].append(dict(case=filename, origin=origin,
            empty=empty, old_empty=group._empty_parenthesis,
            typed=getattr(group, '_typed_parameters', False), pending=pending is not None,
            context=value))


if label == 'candidate':
    sys.setprofile(observe)
    for path in sorted((FROZEN / 'augmented/corpus/affected').glob('*')):
        filename = path.name
        lizard.analyze_file.analyze_source_code(str(path), path.read_text())
    sys.setprofile(None)
    assert not calls
    assert all(not binding_contexts[key] for key in (
        'without_pending', 'without_binding', 'emptiness_mismatches',
        'missing_origin', 'callback_origins', 'eligibility_mismatches')), binding_contexts
for _ in range(5):
    batch()
profiler = cProfile.Profile()
profiler.enable()
for _ in range(1000):
    batch()
profiler.disable()
assert metrics() == before
stats = pstats.Stats(profiler)
rows = []
for (file, line, name), values in stats.stats.items():
    calls, total_calls, own, cumulative, _ = values
    rows.append(dict(file=file, line=line, function=name, calls=calls,
                     total_calls=total_calls, own_seconds=own, cumulative_seconds=cumulative))
rows.sort(key=lambda row: row['own_seconds'], reverse=True)
assert not any(row['function'] == 'return_type_colon' for row in rows)
destination = OUT / ('control2-' + label + '.json')
assert not destination.exists()
profiler.dump_stats(str(destination.with_suffix('.pstats')))
payload = dict(snapshot=str(snapshot), loops=1000, names=NAMES, fields=FIELDS,
               metrics=before, total_seconds=stats.total_tt, rows=rows,
               script_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
               binding_contexts={key: value for key, value in binding_contexts.items() if key != 'rows'})
destination.write_text(json.dumps(payload, indent=2) + '\n')
if label == 'candidate':
    (OUT / 'completed-binding-observations.json').write_text(json.dumps(binding_contexts, indent=2) + '\n')
print(json.dumps(dict(control=label, total_seconds=stats.total_tt, top=rows[:12])), flush=True)
