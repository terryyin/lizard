"""Retain every pair and compact, bounded consumer/evidence records."""
import hashlib
import json
from pathlib import Path
import re
import shutil
import sys

ROOT = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr536-corrected')
CHECKOUT = Path.cwd()
DEST = CHECKOUT / 'docs/reviews/fabio-malpezzi-20261027/pr536/corrected-proof'
DEST.mkdir(exist_ok=False)


def sha(data):
    return hashlib.sha256(data).hexdigest()


def retain(source, name=None):
    content = source.read_bytes()
    lines = content.splitlines(keepends=True)
    target = name or source.name
    escaped = content.endswith((b'\n\n', b'\r\n\r\n')) or any(
        line.rstrip(b'\r\n').endswith((b' ', b'\t')) for line in lines)
    if len(lines) <= 240 and not escaped:
        (DEST / target).write_bytes(content)
    else:
        for number, start in enumerate(range(0, len(lines), 220), 1):
            chunk = b''.join(lines[start:start + 220])
            (DEST / (target + '.part' + str(number) + '.json')).write_text(
                json.dumps({'source': source.name, 'source_sha256': sha(content),
                            'start_line': start + 1, 'chunk_sha256': sha(chunk),
                            'text': chunk.decode()}) + '\n')


files = ['identities.json', 'supplements.json', 'calibration.log',
         'candidate-5-full-pytest.log', 'candidate-5-reproduce.jsonl',
         'candidate-5-semantic.log', 'candidate-5-measure.log',
         'candidate-0-tuples.json', 'candidate-5-tuples.json',
         'candidate-2-target-api-parameters.json', 'candidate-2-target-api-parameters.log']
for number in range(6):
    label = 'candidate-' + str(number)
    files.extend(path.name for path in ROOT.glob(label + '-diagnose.log'))
    files.extend(path.name for path in ROOT.glob(label + '-identity.json'))
    files.extend(path.name for path in ROOT.glob(label + '.patch'))
    files.extend(path.name for path in ROOT.glob(label + '-product.json'))
for label in ['target', 'baseline']:
    files.append(label + '-product.json')
for comparison in ['target', 'baseline']:
    for workload in ['api', 'api-parameters', 'cli', 'cli-ns']:
        label = 'candidate-5-' + comparison + '-' + workload
        files.extend(label + suffix for suffix in ['.json', '.log', '-host.json'])
for filename in sorted(set(files)):
    retain(ROOT / filename)
runtime = {}
for label in ['paired', 'verify', 'measure']:
    source = CHECKOUT / ('docs/reviews/fabio-malpezzi-20261027/pr536/corrected-' + label + '.py')
    executed = Path('/private/tmp/lizard-corrected-' + label + '.py')
    assert source.read_bytes() == executed.read_bytes()
    runtime[str(executed)] = sha(executed.read_bytes())
runtime['interpreter'] = str(Path(sys.executable).resolve())
runtime['python'] = sys.version
for name in ['flake.nix', 'flake.lock']:
    runtime[name] = sha((CHECKOUT / name).read_bytes())
(DEST / 'runtime-scripts.json').write_text(json.dumps(runtime, sort_keys=True) + '\n')
retain(ROOT / 'proof/manifest.json', 'corpus-manifest.json')
for label in ['candidate-0', 'candidate-5']:
    for workload in ['cli', 'cli-ns']:
        retain(ROOT / (label + '-' + workload + '-semantics.log'))

# Verify the original broad bytes against both the frozen review receipt and
# the immutable Git baseline, independently of the historical cache manifest.
original = json.loads((CHECKOUT / 'docs/reviews/fabio-malpezzi-20261027/manifest.json').read_text())
corpus = json.loads((ROOT / 'proof/manifest.json').read_text())
broad = {name: digest for name, digest in corpus.items() if not name.startswith('affected/')}
assert len(broad) == 100
assert broad == {name: digest for name, digest in original['files'].items()
                 if not name.startswith('affected/')}
assert original['revision'] == '2e21e404c1ab9f4e0b49acc80816e816629d0670'
assert all(sha((ROOT / 'baseline' / name).read_bytes()) == digest for name, digest in broad.items())

trace = (ROOT / 'candidate-5-consumers-raw.md').read_text().splitlines()
rows = 0
located = False
for index, line in enumerate(trace):
    if line == '## Located consumers':
        located = True
    match = re.match(r'^\| `([^`]+)` \|', line) if located else None
    if not match:
        continue
    path = match.group(1)
    rows += 1
    if path.startswith('test/') and '/testdata/' not in path:
        disposition = 'run: candidate-5-full-pytest.log; relevant named assertions listed in slice4-proof.md'
    elif path.startswith(('lizard_core/', 'lizard_ext/', 'lizard_languages/')):
        disposition = 'run: full pytest + all101 public tuple parity + broad CLI/default-ENS parity; shared owner/reader or metric consumer'
    elif path == 'index.py' or path == 'scripts/website_smoke.py':
        disposition = 'run: full pytest includes actual website JSON and HTTP smoke suites; FormalSlots count3 and ArrowParameters count2 assertions'
    elif path == 'website/frontend/src/Playground.jsx':
        disposition = 'reused: slice3-proof.md inspected METRICS/fn[field]; unchanged numeric JSON/rendering boundary; no browser claim'
    elif path.startswith('docs/reviews/'):
        disposition = 'excluded: retained review evidence/harness is not a live product consumer; current harness exercised in eight gates'
    elif path.startswith(('docs/', '.planning/', '.agents/', '.claude/')):
        disposition = 'excluded: documentation/lifecycle tooling; no changed runtime parameter operation'
    elif '/testdata/' in path:
        disposition = 'run: unchanged fixture consumed by full language suite; no independent collector or metric consumer'
    else:
        assert path in ['README.rst', 'CHANGELOG.md', 'DearDough.md', 'prompt-history.md'], path
        disposition = 'excluded: historical/product prose; no changed runtime operation or meaning'
    assert line.endswith('| |'), line
    trace[index] = line[:-1] + ' ' + disposition + ' |'
for number, start in enumerate(range(0, len(trace), 205), 1):
    (DEST / ('consumers-part' + str(number) + '.md')).write_text(
        '# Slice4 consumer trace, part ' + str(number) + '\n\n'
        + '\n'.join(trace[start:start + 205]) + '\n')

summaries = []
for comparison in ['target', 'baseline']:
    for workload in ['api', 'api-parameters', 'cli', 'cli-ns']:
        name = 'candidate-5-' + comparison + '-' + workload
        result = json.loads((ROOT / (name + '.json')).read_text())
        assert result['passes']
        assert len(result['raw']) == 20
        durations = [value for row in result['raw'] for value in row.values()]
        assert min(durations) > 1
        summaries.append({'comparison': comparison, 'workload': workload,
                          'loops': result['loops'], 'medians': result['medians'],
                          'stdev': result['stdev'], 'paired_95pct_ci': result['paired_95pct_ci'],
                          'paired_geometric_slowdown_pct': result['paired_geometric_slowdown_pct'],
                          'duration_range': [min(durations), max(durations)]})
(DEST / 'summary.json').write_text(json.dumps({'gates': summaries, 'consumer_rows': rows,
                                              'consumer_trace_raw_sha256': sha((ROOT / 'candidate-5-consumers-raw.md').read_bytes()),
                                              'broad_verified_against_original_and_git': True}) + '\n')
manifest = {str(path.relative_to(DEST)): sha(path.read_bytes()) for path in sorted(DEST.iterdir())}
(DEST / 'evidence-manifest.json').write_text(json.dumps(manifest, sort_keys=True) + '\n')
assert all(len(path.read_bytes().splitlines()) <= 250 for path in DEST.iterdir())
print(json.dumps({'evidence': str(DEST), 'consumer_rows': rows, 'gates': summaries}))
