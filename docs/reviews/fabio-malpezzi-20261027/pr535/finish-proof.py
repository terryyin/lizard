"""Retain final invariants, extra consumers, and whitespace-safe evidence."""
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import sys

REPO = Path(__file__).resolve().parents[4]
EVIDENCE = Path(__file__).resolve().parent
ROOT = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr535')
CACHE = ROOT.parent
trace = REPO / '.agents/skills/dough-execute-plan/scripts/consumer-trace.mjs'
commands = [
    ['node', str(trace), '--checkout', str(REPO), '--term', '.parameters',
     '--term', 'full_parameters'],
    ['node', str(trace), '--checkout', '/private/tmp/lizard-pr532-validation',
     '--term', 'parameter_count', '--term', 'TypeScriptReader',
     '--term', '.parameters', '--term', 'full_parameters',
     '--term', 'function-results'],
]
for index, command in enumerate(commands, 3):
    result = subprocess.run(command, cwd=REPO, text=True,
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    assert result.returncode == 0, result.stdout
    (ROOT / ('consumer-trace-' + str(index) + '.md')).write_text(result.stdout)
    print(json.dumps({'trace': index, 'command': command,
                      'lines': len(result.stdout.splitlines())}), flush=True)

command = [sys.executable, '-m', 'pytest', '-q',
    'test/test_languages/testOptionalOperators.py', 'test/testLanguages.py']
result = subprocess.run(command, cwd=REPO, text=True,
                        stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
(ROOT / 'consumer-supplement.log').write_text(result.stdout)
assert result.returncode == 0, result.stdout
print(json.dumps({'command': command, 'result': result.stdout.splitlines()[-1]}))

def rows(label):
    return [json.loads(line) for line in (ROOT / (label + '-reproduction.log')).read_text().splitlines()
            if '"metrics"' in line]

original, corrected = rows('target'), rows('corrected')
assert [r['case'] for r in original] == [r['case'] for r in corrected]
assert [r['metrics'] for r in original] == [r['metrics'] for r in corrected]
cli = {}
targets = [str(CACHE / 'corpus' / name) for name in
           ('lizard.py', 'lizard_core', 'lizard_languages', 'lizard_ext')]
for label in ('target', 'corrected'):
    cli[label] = {}
    for mode, options in [('cli', []), ('cli-ns', ['-ENS'])]:
        result = subprocess.run([sys.executable, str(ROOT / label / 'lizard.py')]
            + options + targets, cwd=str(ROOT / label), stdout=subprocess.PIPE,
            stderr=subprocess.PIPE)
        assert result.returncode in (0, 1), result.stderr
        cli[label][mode] = {'returncode': result.returncode,
            'sha256': hashlib.sha256(result.stdout).hexdigest()}
assert cli['target'] == cli['corrected']
manifest = json.loads((EVIDENCE.parent / 'manifest.json').read_text())['files']
broad = {name: value for name, value in manifest.items() if not name.startswith('affected/')}
assert len(broad) == 100
for name, value in broad.items():
    for folder in (CACHE / 'corpus', ROOT / 'affected/corpus', ROOT / 'consumers/corpus'):
        assert hashlib.sha256((folder / name).read_bytes()).hexdigest() == value, name
unchanged_ns = ['lizard_core/function_info.py', 'lizard_core/file_analyzer.py',
                'lizard_core/cli.py', 'lizard_ext/lizardns.py', 'lizard_languages/python.py']
for name in unchanged_ns:
    assert (ROOT / 'target' / name).read_bytes() == (ROOT / 'corrected' / name).read_bytes()
    assert (ROOT / 'corrected' / name).read_bytes() == (CACHE / 'pr532/corrected' / name).read_bytes()
invariants = {'metrics_equal': True, 'cases': len(original), 'broad_files': len(broad),
    'broad_outputs': cli, 'unchanged_ns_owners': unchanged_ns,
    'ns_api_reuse': 'The Python-only API corpus never enters the changed JavaScript _dec path; FunctionInfo, Python reader and NS extension match the previously measured combined candidate. CLI -ENS startup is measured directly again.',
    'ns_fresh_interval': json.loads((ROOT / 'cumulative-cli-ns.json').read_text())['paired_95pct_ci']}
(ROOT / 'final-invariants.json').write_text(json.dumps(invariants, indent=2) + '\n')
patches = {}
for label, first, second in [
    ('raw-child-applied', ROOT / 'target', ROOT / 'raw-child'),
    ('corrected', ROOT / 'target', ROOT / 'corrected')]:
    relative = 'lizard_languages/javascript_functions.py'
    result = subprocess.run(['git', 'diff', '--no-index', str(first / relative),
                             str(second / relative)], text=True, stdout=subprocess.PIPE)
    assert result.returncode == 1
    patches[label] = {'sources': [str(first / relative), str(second / relative)],
                      'patch': result.stdout}
    normalized = '\n'.join(line.rstrip() for line in result.stdout.splitlines()).rstrip() + '\n'
    (EVIDENCE / (label + '.diff')).write_text(
        '# DISPLAY ONLY: normalized whitespace; exact bytes in literal-git-patches.json\n' + normalized)
for label, revisions in [('full', ['01afe9c', '171079e']),
                          ('child', ['171079e^', '171079e'])]:
    patch = subprocess.check_output(['git', 'diff'] + revisions, cwd=REPO, text=True)
    patches[label] = {'revisions': revisions, 'patch': patch}
    normalized = '\n'.join(line.rstrip() for line in patch.splitlines()).rstrip() + '\n'
    (EVIDENCE / (label + '.diff')).write_text(
        '# DISPLAY ONLY: normalized whitespace; exact bytes in literal-git-patches.json\n' + normalized)
(EVIDENCE / 'literal-git-patches.json').write_text(json.dumps(patches) + '\n')
assert json.loads((EVIDENCE / 'literal-git-patches.json').read_text()) == patches
workflow = '.github/workflows/python-app.yml'
(EVIDENCE / 'required-workflow.yml').write_text((ROOT / 'target' / workflow).read_text().rstrip() + '\n')

def disposition(path):
    if path.startswith('docs/reviews/'):
        if '/pr535/' in path and path.endswith('.py'):
            return 'run: literal README commands; bootstrap uses immutable Git reader bytes, never normalized display patches'
        if '/pr535/' in path or 'consumer-trace' in path:
            return 'excluded: proof/trace record itself; no product caller'
        if path.endswith('.py'):
            return 'reused: historical executor replay remains pinned to its recorded immutable source/harness; current harness unchanged'
        return 'excluded: historical evidence snapshot, not a current product caller'
    if path.startswith('.planning/') or path.startswith('.agents/') or path in ('README.rst', 'CHANGELOG.md'):
        return 'excluded: guidance/history names the concept but does not execute the reader'
    if path in ('index.py', 'website/static/demo.js', 'templates/index.html', 'test/test_website.py'):
        return 'run: coordinator owns fresh current-base HTTP/full-suite observation; pending at implementation return'
    if path.startswith('website/'):
        return 'excluded: legacy selected-checkout controller was removed from current target; actual adapter traced separately'
    if path.startswith('test/test_languages/') and not any(term in path for term in
            ('JavaScript', 'TypeScript', 'TSX', 'ES6', 'Vue', 'OptionalOperators')):
        return 'excluded: another language reader; FunctionInfo/other readers are unchanged'
    if path == 'test/testCommentOptions.py':
        return 'excluded: C++ comment forgiveness fixture cannot enter the JS parameter reader'
    if path.startswith('test/') or path.startswith('lizard_languages/') or path.startswith('lizard_core/') or path.startswith('lizard_ext/'):
        return 'run: focused628/5, supplemental consumers, new public output/CLI tests and34-case snapshot proof'
    return 'excluded: no execution of or observation of this reader effect'

for path in sorted(ROOT.glob('consumer-trace-*.md')):
    lines = []
    for line in path.read_text().splitlines():
        match = re.match(r'^\| `([^`]+)` \|.*\| \|$', line)
        if match:
            line = line[:-2] + disposition(match.group(1)) + ' |'
        lines.append(line.rstrip())
    path.write_text('\n'.join(lines).rstrip() + '\n')

copied = []
for path in sorted(ROOT.glob('*')):
    if not path.is_file() or path.name in ('all-inputs.json', 'initial-hashes.json'):
        continue
    if path.suffix not in ('.json', '.log', '.md'):
        continue
    text = '\n'.join(line.rstrip() for line in path.read_text().splitlines()).rstrip() + '\n'
    lines = text.splitlines()
    if path.suffix == '.json' and len(lines) > 230:
        lines = [json.dumps(json.loads(text), separators=(',', ':'))]
    parts = [lines[i:i + 230] for i in range(0, len(lines), 230)]
    for index, part in enumerate(parts, 1):
        name = path.name if len(parts) == 1 else path.name + '.part' + str(index)
        (EVIDENCE / name).write_text('\n'.join(part).rstrip() + '\n')
        copied.append(name)
print(json.dumps({'retained': copied, 'invariants': invariants}), flush=True)
for path in EVIDENCE.iterdir():
    if path.is_file():
        normalized = '\n'.join(line.rstrip() for line in path.read_text().splitlines()).rstrip() + '\n'
        path.write_text(normalized)
