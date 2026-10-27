"""Trace proposal consumers and retain normalized, replayable terminal proof."""
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import sys

REPO = Path(__file__).resolve().parents[4]
EVIDENCE = Path(__file__).resolve().parent
ROOT = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr536')
trace = REPO / '.agents/skills/dough-execute-plan/scripts/consumer-trace.mjs'
commands = [
    ['node', str(trace), '--checkout', str(REPO), '--path',
     'lizard_languages/javascript_bindings.py', '--path',
     'lizard_languages/typescript_states.py', '--term', 'TypeScriptStates',
     '--term', 'JavaScriptBindings', '--term', 'parameter_count',
     '--term', '.parameters', '--term', 'full_parameters'],
    ['node', str(trace), '--checkout', '/private/tmp/lizard-pr532-validation',
     '--term', 'parameter_count', '--term', '.parameters',
     '--term', 'TypeScriptReader', '--term', 'function-results'],
]
for number, command in enumerate(commands, 1):
    result = subprocess.run(command, cwd=REPO, text=True,
        stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    assert result.returncode == 0, result.stdout
    (ROOT / ('consumer-trace-' + str(number) + '.md')).write_text(result.stdout)
    print(json.dumps({'trace': number, 'command': command}), flush=True)

snapshot = ROOT / 'adapted'
files = ['test/test_analyzer.py', 'test/testOutput.py', 'test/testApplication.py',
    'test/test_options.py', 'test/testLanguages.py', 'test/test_extensions',
    'test/test_languages/testVue.py']
command = [sys.executable, '-m', 'pytest', '-q'] + files
result = subprocess.run(command, cwd=snapshot,
    env=dict(os.environ, PYTHONPATH=str(snapshot)), text=True,
    stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
(ROOT / 'supplemental-consumers.log').write_text(result.stdout)
print(json.dumps({'command': command, 'returncode': result.returncode,
                  'last_line': result.stdout.splitlines()[-1]}), flush=True)

# Confirm module identity and observations again after timing collection.
for label in ('parent', 'target', 'raw', 'adapted'):
    command = [sys.executable, str(EVIDENCE / 'reproduce.py'),
               str(ROOT / label), str(ROOT / 'all-inputs.json')]
    result = subprocess.run(command, cwd='/private/tmp',
        env=dict(os.environ, PYTHONPATH=str(ROOT / label)), text=True,
        stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    original = (ROOT / (label + '-reproduction.log')).read_text()
    assert result.stdout == original, label
    (ROOT / (label + '-after-timing.log')).write_text(result.stdout)
manifest = json.loads((EVIDENCE / 'manifest.json').read_text())
for name, digest in manifest.items():
    assert hashlib.sha256((ROOT / 'proof/corpus' / name).read_bytes()).hexdigest() == digest
source_hashes = json.loads((ROOT / 'source-hashes.json').read_text())
for label, hashes in source_hashes.items():
    for name, digest in hashes.items():
        assert hashlib.sha256((ROOT / label / name).read_bytes()).hexdigest() == digest
for name in source_hashes['target']:
    assert (ROOT / 'target' / name).read_bytes() == (
        ROOT.parent / 'pr535/corrected' / name).read_bytes(), name
invariants = {'unchanged_sources': True, 'unchanged_corpus': True,
    'accepted535_product_sources_equal': len(source_hashes['target']),
    'cumulative_reuse': 'No536 product code lands; all100 current-target Python source bytes equal accepted535 corrected snapshot; its incremental/cumulative API, parameters, CLI and CLI-ENS gates remain applicable.'}
(ROOT / 'final-invariants.json').write_text(json.dumps(invariants) + '\n')
print(json.dumps(invariants), flush=True)


def disposition(path):
    if path.startswith('docs/reviews/'):
        if '/pr536/' in path and path.endswith('.py'):
            return 'run: README replay commands; bootstrap applies exact immutable Git patch, never normalized display diff'
        return 'excluded: historical evidence or trace/proof record; no current product caller'
    if path.startswith(('.planning/', '.agents/')) or path in ('README.rst', 'CHANGELOG.md'):
        return 'excluded: guidance/history naming the concept; no runtime parser call'
    if path in ('index.py', 'website/static/demo.js', 'templates/index.html', 'test/test_website.py'):
        return 'excluded: current-target HTTP/UI adapter; no536 code lands and no HTTP behavior accepted'
    if path.startswith('website/'):
        return 'excluded: selected checkout legacy website removed from current target; active adapter traced separately'
    if path.startswith('test/test_languages/') and not any(term in path for term in
            ('JavaScript', 'TypeScript', 'TSX', 'ES6', 'Vue', 'OptionalOperators')):
        return 'excluded: another language cannot enter changed JS states; FunctionInfo unchanged; five explicit other-language controls observed separately'
    if path == 'test/testCommentOptions.py':
        return 'excluded: C++ forgiveness fixture cannot enter JS binding reader'
    if path.startswith(('test/', 'lizard_core/', 'lizard_ext/', 'lizard_languages/')):
        return 'run: snapshot reader groups, supplemental analyzer/output/CLI/options/extensions/registry/Vue, public65-case names/count/CCN and CLI proof'
    return 'excluded: no runtime use of proposed JS arrow effect'


for path in sorted(ROOT.glob('consumer-trace-*.md')):
    rows = []
    for line in path.read_text().splitlines():
        match = re.match(r'^\| `([^`]+)` \|.*\| \|$', line)
        if match:
            line = line[:-2] + disposition(match.group(1)) + ' |'
        rows.append(line.rstrip())
    path.write_text('\n'.join(rows).rstrip() + '\n')
for path in sorted(ROOT.iterdir()):
    if not path.is_file() or path.suffix not in ('.json', '.log', '.md'):
        continue
    text = '\n'.join(line.rstrip() for line in path.read_text().splitlines()).rstrip() + '\n'
    rows = text.splitlines()
    if path.suffix == '.json' and len(rows) > 230:
        rows = [json.dumps(json.loads(text), separators=(',', ':'))]
    parts = [rows[i:i + 230] for i in range(0, len(rows), 230)]
    for number, part in enumerate(parts, 1):
        name = path.name if len(parts) == 1 else path.name + '.part' + str(number)
        (EVIDENCE / name).write_text('\n'.join(part).rstrip() + '\n')
print(json.dumps({'retained': len(list(EVIDENCE.iterdir()))}), flush=True)
