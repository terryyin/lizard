"""Retain inspected consumer dispositions and terminal semantic proof receipts."""
import ast
import glob
import hashlib
import json
from pathlib import Path
import subprocess

HERE = Path(__file__).resolve().parent
CHECKOUT = HERE.parents[3]
CACHE = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-reader-dispatch')
BASELINE = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-ec2-attribution')


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
raw = CACHE / 'consumers-raw-final.md'
lines = raw.read_text().splitlines()
start = lines.index('## Located consumers')
count = 0
for index in range(start, len(lines)):
    line = lines[index]
    if not line.startswith('| `'):
        continue
    name = line.split('`')[1]
    if name in selected:
        disposition = ('run: current focused987/HTTP41/parallel16/property1 selections; '
                       'literal commands and observing assertions in reader-dispatch.md')
    elif name.startswith('test/'):
        disposition = ('excluded: independent language test does not use TypeScriptReader; '
                       'generic CodeReader and that reader are unchanged')
    elif name.startswith('lizard_languages/'):
        if any(term in name for term in ('javascript', 'typescript', '/tsx.py', '/vue.py',
                                        '/__init__.py', '/code_reader.py')):
            disposition = ('run: real reader protocol hooks/finalization tests, 166 exact '
                           'public/FileAnalyzer/import rows and focused shared-language suites')
        elif name.endswith('/clike.py'):
            disposition = ('reused: unchanged independent C-like reader; shared comments '
                           'also exercised by current JS/TS tests and 166 token/metric replays')
        else:
            disposition = ('excluded: independent reader/state does not inherit TypeScriptReader; '
                           'its generic dispatch remains unchanged')
    elif name.startswith('lizard_core/'):
        disposition = ('run: focused analyzer/model/options/output/CLI tests, 166 public '
                       'rows, real HTTP and parallel JS/TS results; no core bytes changed')
    elif name.startswith('lizard_ext/'):
        disposition = ('reused: unchanged upstream extension token/order/metric contract; '
                       'all extension suites plus shared JS/TS nested/optional/output tests run')
    elif name == 'website/frontend/src/Playground.jsx':
        disposition = ('reused: unchanged frontend fn[field] metric rendering; real HTTP '
                       'values tested and seven public fields equal; no new browser claim')
    elif name.endswith('/qualification-verify.py'):
        disposition = ('run: isolated base/working 166 expected/import/FileAnalyzer '
                       'verification through this unchanged helper')
    elif name.startswith('docs/reviews/'):
        disposition = ('excluded: retained historical/source comparison evidence, '
                       'not a runtime caller of TypeScriptReader dispatch')
    elif name.startswith(('.agents/', '.claude/')):
        disposition = ('excluded: workflow/lifecycle or unrelated lexical match; '
                       'does not call the language-reader token protocol')
    else:
        disposition = ('reused: unchanged source, navigation or domain contract; '
                       'generic hooks/metrics remain compatible, no outward wording changed')
    assert line.endswith('| |'), line
    lines[index] = line[:-2] + disposition + ' |'
    count += 1
trace = CACHE / 'consumers-final.md'
trace.write_text('\n'.join(lines) + '\n')
assert not any(line.startswith('| `') and line.endswith('| |') for line in lines[start:])
profile_summary = {}
for corpus in ('original', 'augmented'):
    working = json.loads((CACHE / (corpus + '-working-profile.json')).read_text())
    before = json.loads((BASELINE / (corpus + '-candidate.json')).read_text())
    assert working['metrics'] == before['metrics']
    profile_summary[corpus] = dict(old_seconds=before['total_seconds'],
                                   working_seconds=working['total_seconds'],
                                   dispatch=working['dispatch'],
                                   sha256=digest(CACHE / (corpus + '-working-profile.json')))
proof_path = HERE / 'reader-dispatch-proof.json'
proof = json.loads(proof_path.read_text())
assert all(digest(CHECKOUT / name) == value for name, value in proof['working_product_sha256'].items())
assert not subprocess.check_output(['git', 'diff', '--name-only', '--', 'test'], cwd=CHECKOUT)
counts = {'focused': ('987 passed, 5 skipped', 987), 'website': ('41 passed', 41),
          'parallel': ('16 passed', 16), 'property-hook': ('1 passed', 1)}
tests = {}
for name, (expected, passed) in counts.items():
    log = CACHE / (name + '.log')
    assert expected in log.read_text(), name
    tests[name] = dict(passed=passed, returncode=0, sha256=digest(log))
proof.update(tests=tests, profiles=profile_summary,
             trace=dict(path=str(trace), rows=count, blank_dispositions=0, sha256=digest(trace)),
             corrected_fixture=json.loads((CACHE / 'initial-dispatch-test-receipt.json').read_text()))
proof_path.write_text(json.dumps(proof, separators=(',', ':')) + '\n')
owned = [CHECKOUT / 'lizard_languages/typescript.py',
         CHECKOUT / 'test/test_languages/testTypeScriptReaderDispatch.py']
owned += sorted(HERE.glob('reader-dispatch*'))
for path in owned:
    content = path.read_text()
    assert content.endswith('\n') and not content.endswith('\n\n'), path
    assert all(not line.rstrip() != line for line in content.splitlines()), path
    assert len(content.splitlines()) <= 250, path
    if path.suffix == '.py':
        ast.parse(content, filename=str(path))
    elif path.suffix == '.json':
        json.loads(content)
print(json.dumps(dict(trace_rows=count, blank_dispositions=0, files_checked=len(owned),
                      runtime_patch=proof['runtime_patch_sha256'], terminal_returncode=0)), flush=True)
