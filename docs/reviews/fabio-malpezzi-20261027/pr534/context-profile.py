"""Bounded owner-query attribution; this is not a throughput qualification."""

import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys

HERE = Path(__file__).resolve().parent
CACHE = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-context')
FROZEN = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-optimized-8dac7b80')
CHECKOUT = Path('/Users/terryyin/git/lizard/.worktrees/preserve-arrow-boundaries-across-ternary-and-typ')

if '--worker' not in sys.argv:
    CACHE.mkdir(exist_ok=True)
    temporary = Path('/private/tmp/lizard-seed015-context-profile.py')
    shutil.copyfile(__file__, temporary)
    labels = sys.argv[1:] or ['current', 'candidate']
    for label in labels:
        subprocess.run([sys.executable, str(temporary), '--worker', label],
                       cwd='/private/tmp', check=True)
    raise SystemExit(0)

import cProfile
import profile
import pstats

label = sys.argv[2]
snapshot = CHECKOUT if label.startswith('working') else FROZEN / label
destination = CACHE / (label + '-profile.json')
assert not destination.exists(), destination
assert not Path(profile.__file__).resolve().is_relative_to(snapshot)
sys.path.insert(0, str(snapshot))
import lizard
from lizard_languages import typescript

assert Path(lizard.__file__).resolve().is_relative_to(snapshot)
assert Path(typescript.__file__).resolve().is_relative_to(snapshot)
cases = json.loads((FROZEN / 'inputs.json').read_text())
fields = ('name', 'start_line', 'end_line', 'cyclomatic_complexity',
          'nloc', 'token_count', 'parameter_count')


def analyze():
    return {case['filename']: [[getattr(function, field) for field in fields]
                              for function in lizard.analyze_file.analyze_source_code(
                                  case['filename'], case['source']).function_list]
            for case in cases}


before = analyze()
counts = dict(queries=0, root_after_group=0, accepted=0, accepted_without_group=0)


def observe(frame, event, result):
    if event == 'return' and frame.f_code.co_name == 'return_annotation_context':
        counts['queries'] += 1
        after_group = frame.f_locals['self'].last_token == ')'
        counts['root_after_group'] += after_group
        counts['accepted'] += result is not None
        counts['accepted_without_group'] += result is not None and not after_group


sys.setprofile(observe)
observed = analyze()
sys.setprofile(None)
assert observed == before
assert not counts['accepted_without_group'], counts
for _ in range(5):
    analyze()
profiler = cProfile.Profile()
profiler.enable()
for _ in range(100):
    analyze()
profiler.disable()
assert analyze() == before
stats = pstats.Stats(profiler)
rows = []
for (file, line, name), values in stats.stats.items():
    calls, total_calls, own, cumulative, _ = values
    rows.append(dict(file=file, line=line, function=name, calls=calls,
                     total_calls=total_calls, own_seconds=own,
                     cumulative_seconds=cumulative))
rows.sort(key=lambda row: row['own_seconds'], reverse=True)
profiler.dump_stats(str(CACHE / (label + '.pstats')))
payload = dict(snapshot=str(snapshot), loops=100, cases=len(cases), fields=fields,
               metrics=before, total_seconds=stats.total_tt,
               total_calls=stats.total_calls, context=counts,
               host=os.uname().nodename, load=os.getloadavg(),
               imports=dict(facade=lizard.__file__, reader=typescript.__file__),
               source_sha256=hashlib.sha256(Path(typescript.__file__).read_bytes()).hexdigest(),
               rows=rows)
destination.write_text(json.dumps(payload, indent=2) + '\n')
selected = [row for row in rows if row['function'] in
            ('preprocess', 'return_annotation_context', 'return_type_colon',
             '_has_branch_separators', '_state_global')]
print(json.dumps(dict(label=label, total_seconds=stats.total_tt,
                      context=counts, selected=selected, top=rows[:10])), flush=True)
