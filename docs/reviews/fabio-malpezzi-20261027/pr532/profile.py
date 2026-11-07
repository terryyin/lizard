"""One bounded CLI profile; output equality is checked before interpretation."""
import hashlib
import json
from pathlib import Path
import pstats
import subprocess
import sys

ROOT = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr532')
CORPUS = ROOT / 'affected/corpus'
profiles = {}
hashes = {}
for name in ('target', 'corrected'):
    command = [sys.executable, '-m', 'cProfile', '-o', str(ROOT / (name + '.prof')),
               str(ROOT / name / 'lizard.py')] + [str(CORPUS / x) for x in
               ('lizard.py', 'lizard_core', 'lizard_languages', 'lizard_ext')]
    # The snapshots contain a Python 2 profile.py; keep it out of stdlib imports.
    result = subprocess.run(command, cwd='/private/tmp', stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE)
    (ROOT / (name + '-profile-stderr.log')).write_bytes(result.stderr)
    (ROOT / (name + '-profile-command.json')).write_text(json.dumps(command) + '\n')
    assert result.returncode in (0, 1), result.stderr
    assert not result.stderr, result.stderr.decode()
    assert (ROOT / (name + '.prof')).exists(), result.stderr.decode()
    (ROOT / (name + '-profile-output.log')).write_bytes(result.stdout)
    hashes[name] = hashlib.sha256(result.stdout).hexdigest()
    stats = pstats.Stats(str(ROOT / (name + '.prof')))
    for relative in ('lizard.py', 'lizard_languages/__init__.py',
                     'lizard_languages/javascript_functions.py',
                     'lizard_languages/typescript_states.py',
                     'lizard_core/function_info.py'):
        assert any(key[0] == str(ROOT / name / relative) for key in stats.stats), relative
    rows = sorted(stats.stats.items(), key=lambda item: item[1][3], reverse=True)
    selected = rows[:25] + [(key, value) for key, value in rows[25:]
                            if key[0].endswith('function_info.py')]
    profiles[name] = [dict(file=key[0], line=key[1], function=key[2],
                           calls=value[1], own=value[2], cumulative=value[3])
                      for key, value in selected]
assert hashes['target'] == hashes['corrected'], hashes
(ROOT / 'cli-profile.json').write_text(json.dumps(dict(
    output_hashes=hashes, profiles=profiles,
    limit='One profile per snapshot. Scheduling/CPU contention cannot be attributed from this alone.'),
    separators=(',', ':')) + '\n')
print(json.dumps({'output_equal': True, 'hashes': hashes,
                  'profile': str(ROOT / 'cli-profile.json')}))
