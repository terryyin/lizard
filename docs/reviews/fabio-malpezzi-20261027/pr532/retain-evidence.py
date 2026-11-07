"""Keep compact terminal copies while preserving original external logs."""
import json
from pathlib import Path
import re

REPO = Path(__file__).resolve().parents[4]
HERE = Path(__file__).resolve().parent
ROOT = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr532')
names = ['target-red.log', 'raw-target-red.log', 'corrected-focused.log',
         'raw-target-focused.log', 'consumer-focused.log',
         'target-final-reproduction.log', 'raw-target-final-reproduction.log',
         'corrected-final-reproduction.log', 'baseline-final-reproduction.log',
         'default-equivalence.json', 'snapshot-hashes.json', 'inputs-manifest.json',
         'cli-profile.json', 'profile-diagnostic.log', 'profile-failed-stderr.log',
         'target-profile-command.json', 'corrected-profile-command.json',
         'long-headroom.json', 'ns-headroom.json', 'harness-replay.json', 'website-consumer.json',
         'baseline-supplemental.log', 'target-supplemental.log',
         'raw-target-supplemental.log', 'corrected-supplemental.log',
         'dependency-535-supplemental.log', 'target-preexisting.log',
         'raw-target-preexisting.log']
for label in ('raw-api', 'raw-cli', 'corrected-api', 'corrected-parameters', 'corrected-cli'):
    names += [label + '.json', label + '-timings.log']
names += ['corrected-cli-long.json', 'corrected-cli-long-timings.log']
names += ['cumulative-cli-ns.json', 'cumulative-cli-ns-timings.log']
for name in names:
    source = ROOT / name
    if not source.exists():
        continue
    text = re.sub(r'\x1b\[[0-9;]*[A-Za-z]', '', source.read_text())
    lines = [line.rstrip() for line in text.splitlines()]
    if len(lines) <= 230:
        (HERE / name).write_text('\n'.join(lines).rstrip() + '\n')
    else:
        for offset in range(0, len(lines), 230):
            path = HERE / (source.stem + '.part' + str(offset // 230 + 1) + source.suffix)
            path.write_text('\n'.join(lines[offset:offset + 230]).rstrip() + '\n')
for kind in ('affected', 'consumers'):
    (HERE / (kind + '-manifest.json')).write_bytes((ROOT / kind / 'manifest.json').read_bytes())
print(json.dumps({'retained': len(names), 'external_originals': str(ROOT)}))
