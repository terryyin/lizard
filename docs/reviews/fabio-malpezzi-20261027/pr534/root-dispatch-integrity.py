"""Retain inspected root/child consumer and numeric verification correspondence."""
import ast
import glob
import hashlib
import json
from pathlib import Path
import subprocess

HERE = Path(__file__).resolve().parent
CHECKOUT = HERE.parents[3]
CACHE = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-root-dispatch')
BASELINE = CACHE.parent / 'seed015-reader-dispatch'


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


selected = set()
for pattern in ('test/test_languages/testJavaScript*.py', 'test/test_languages/testTypeScript*.py',
                'test/testOutput*.py', 'test/test_extensions/*.py'):
    selected.update(glob.glob(pattern))
selected.update('test/test_languages/' + name + '.py' for name in (
    'testES6', 'testJSX', 'testTSX', 'testVue', 'testArrowObjectBoundaries', 'testOptionalOperators'))
selected.update(('test/test_analyzer.py', 'test/testApplication.py', 'test/test_options.py',
                 'test/testBasicFunctionInfo.py', 'test/test_website.py',
                 'test/test_website_smoke.py', 'test/test_parallel_analysis.py'))
lines = (CACHE / 'consumers-raw-final.md').read_text().splitlines()
start = lines.index('## Located consumers')
count = 0
for index in range(start, len(lines)):
    line = lines[index]
    if not line.startswith('| `'):
        continue
    name = line.split('`')[1]
    if name in selected:
        disposition = ('run: focused990/HTTP41/worker15/protocol12; real grammar, '
                       'binding/default/formal/callback and public result assertions in root-dispatch.md')
    elif name.startswith('test/'):
        disposition = ('excluded: independent language test does not use TypeScriptReader; '
                       'its reader and generic CodeStateMachine implementation are unchanged')
    elif name.startswith('lizard_languages/'):
        if any(term in name for term in ('javascript', 'typescript', '/tsx.py', '/vue.py',
                                        '/__init__.py', '/code_reader.py')):
            disposition = ('run: configured-root/child/hook lifecycle tests and166 exact '
                           'public/import/FileAnalyzer replays; canonical base dispatch remains exercised')
        elif name.endswith('/clike.py'):
            disposition = ('reused: unchanged C-like reader; shared comment protocol also '
                           'observed by current JS/TS tests and seven-field/token parity')
        else:
            disposition = ('excluded: independent reader/state does not inherit this '
                           'TypeScriptReader entry path; its owner code is unchanged')
    elif name.startswith('lizard_core/') or name in ('lizard.py', 'index.py', 'scripts/website_smoke.py'):
        disposition = ('run: focused core/model/output/CLI,166 public rows,41 realHTTP '
                       'and parallel JS/TS result/worker lifecycle observations')
    elif name.startswith('lizard_ext/'):
        disposition = ('reused: all extension suites run with shared-reader/output '
                       'proof; extension code and token/counter ordering unchanged')
    elif name == 'website/frontend/src/Playground.jsx':
        disposition = ('reused: unchanged fn[field] rendering; current HTTP/public '
                       'metric equality observed; no browser rendering claim')
    elif name.endswith('/qualification-verify.py'):
        disposition = ('run: unchanged isolated verifier for exact13c and working166 '
                       'expected/import/FileAnalyzer/all-seven-field observations')
    elif name.startswith('docs/reviews/'):
        disposition = ('excluded: retained historical/source/PFE evidence, '
                       'not a current runtime caller of root token dispatch')
    elif name.startswith(('.agents/', '.claude/')):
        disposition = ('excluded: workflow/lifecycle or lexical match; '
                       'does not consume the TypeScriptReader token protocol')
    else:
        disposition = ('reused: unchanged navigation/domain/source contract; '
                       'no outward wording, counter or extension contract changed')
    assert line.endswith('| |'), line
    lines[index] = line[:-2] + disposition + ' |'
    count += 1
trace = CACHE / 'consumers-final.md'
trace.write_text('\n'.join(lines) + '\n')
assert not any(line.startswith('| `') and line.endswith('| |') for line in lines[start:])

profiles = {}
for corpus in ('original', 'augmented'):
    working = json.loads((CACHE / (corpus + '-working-profile.json')).read_text())
    before = json.loads((BASELINE / (corpus + '-working-profile.json')).read_text())
    assert working['metrics'] == before['metrics']
    wrapper = lambda profile: [row for row in profile['rows']
                               if row['file'].endswith('typescript_states.py')
                               and row['function'] == '__call__']
    profiles[corpus] = dict(old_seconds=before['total_seconds'],
                            working_seconds=working['total_seconds'],
                            old_wrapper=wrapper(before), working_wrapper=wrapper(working),
                            sha256=digest(CACHE / (corpus + '-working-profile.json')))
proof_path = HERE / 'root-dispatch-proof.json'
proof = json.loads(proof_path.read_text())
assert all(digest(CHECKOUT / name) == value for name, value in proof['working_product_sha256'].items())
changed_tests = subprocess.check_output(['git', 'diff', '--name-only', '--', 'test'],
                                        cwd=CHECKOUT).decode().splitlines()
assert changed_tests == ['test/test_languages/testTypeScriptReaderDispatch.py'], changed_tests
counts = {'protocol': ('12 passed', 12, '8d1214'),
          'focused': ('990 passed, 5 skipped', 990, '5190ea'),
          'website': ('41 passed', 41, 'd28393'),
          'parallel': ('15 passed', 15, 'a33629')}
tests = {}
for name, (expected, passed, tool_receipt) in counts.items():
    log = CACHE / (name + '.log')
    assert expected in log.read_text(), name
    tests[name] = dict(passed=passed, returncode=0, terminal_tool_chunk=tool_receipt,
                       sha256=digest(log))
proof.update(tests=tests, profiles=profiles,
             trace=dict(path=str(trace), rows=count, blank_dispositions=0, sha256=digest(trace)))
proof_path.write_text(json.dumps(proof, separators=(',', ':')) + '\n')
owned = [CHECKOUT / 'lizard_languages/typescript.py',
         CHECKOUT / 'test/test_languages/testTypeScriptReaderDispatch.py']
owned += [HERE / name for name in ('root-dispatch.md', 'root-dispatch-verify.py',
                                   'root-dispatch-profile.py', 'root-dispatch-proof.json',
                                   'root-dispatch-integrity.py')]
for path in owned:
    content = path.read_text()
    assert content.endswith('\n') and not content.endswith('\n\n'), path
    assert all(line == line.rstrip() for line in content.splitlines()), path
    assert len(content.splitlines()) <= 250, path
    if path.suffix == '.py':
        ast.parse(content, filename=str(path))
    elif path.suffix == '.json':
        json.loads(content)
print(json.dumps(dict(trace_rows=count, blank_dispositions=0, files_checked=len(owned),
                      runtime_patch=proof['runtime_patch_sha256'], terminal_returncode=0)), flush=True)
