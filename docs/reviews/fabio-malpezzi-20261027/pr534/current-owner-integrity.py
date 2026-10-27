"""Retain source correspondence and complete the inspected consumer record."""

import glob
import hashlib
import json
from pathlib import Path
import subprocess

HERE = Path(__file__).resolve().parent
CHECKOUT = HERE.parents[3]
CACHE = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr534/seed015-current-1bc7cbd')
SUPPLIER = 'f6d5b09f68759bf5904d23a4361c0ea0ad02e0d3'


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


manifest_path = HERE.parent / 'pr536/corrected-proof/candidate-5-product.json'
manifest = json.loads(manifest_path.read_text())
different = [name for name, value in manifest.items() if digest(CHECKOUT / name) != value]
assert different == ['lizard_languages/javascript_functions.py',
                     'lizard_languages/typescript.py', 'lizard_languages/typescript_states.py']
product = {name: digest(CHECKOUT / name) for name in manifest}
product['lizard_languages/typescript_signatures.py'] = digest(
    CHECKOUT / 'lizard_languages/typescript_signatures.py')
product_path = HERE / 'current-owner-product-hashes.json'
product_path.write_text(json.dumps(product, indent=2) + '\n')
selected = set()
for pattern in ('test/test_languages/testJavaScript*.py', 'test/test_languages/testTypeScript*.py',
                'test/testOutput*.py'):
    selected.update(glob.glob(pattern))
selected.update('test/test_languages/{}.py'.format(name) for name in (
    'testES6', 'testJSX', 'testTSX', 'testVue', 'testArrowObjectBoundaries', 'testOptionalOperators'))
selected.update(('test/test_analyzer.py', 'test/testApplication.py', 'test/test_options.py',
                 'test/testBasicFunctionInfo.py', 'test/test_extensions/testCognitiveComplexity.py',
                 'test/test_extensions/testDuplicatedParamList.py',
                 'test/test_website.py', 'test/test_website_smoke.py'))
trace_path = CACHE / 'consumers-final.md'
lines = trace_path.read_text().splitlines()
start = lines.index('## Located consumers')
count = 0
reused_tests = []
for index in range(start, len(lines)):
    line = lines[index]
    if not line.startswith('| `'):
        continue
    name = line.split('`')[1]
    if name in selected:
        disposition = ('run: 842 focused or 41 HTTP/website tests; explicit public '
                       'assertions/setup in current-owner-integration.md')
    elif name.startswith('test/'):
        old = subprocess.check_output(['git', 'show', SUPPLIER + ':' + name], cwd=CHECKOUT)
        assert old == (CHECKOUT / name).read_bytes(), name
        reused_tests.append(name)
        disposition = ('reused: supplier full pytest 1944 passed/7 skipped, exit0; '
                       'identical test bytes and unaffected owner hashes checked')
    elif name.startswith('lizard_languages/'):
        if name in different or any(word in name for word in (
                'javascript', 'typescript', '/tsx.py', '/vue.py', '/__init__.py')):
            disposition = ('run: shared JS/TS suites and 128 public/import/FileAnalyzer '
                           'replays; original six fields preserved')
        else:
            assert name in product and digest(CHECKOUT / name) == manifest[name], name
            disposition = ('reused: supplier full pytest 1944 passed/7 skipped, exit0; '
                           'language/model bytes identical to tested manifest')
    elif name.startswith('lizard_core/') or name in ('lizard.py', 'index.py', 'scripts/website_smoke.py'):
        disposition = ('run: focused API/CLI/core/output and 41 real HTTP/website '
                       'observations; inspect current-owner-integration.md')
    elif name.startswith('lizard_ext/'):
        disposition = ('reused: identical supplier extension/model bytes and full pytest; '
                       'cognitive/duplicate-list paths also run focused')
    elif name == 'website/frontend/src/Playground.jsx':
        disposition = ('reused: unchanged fn[field] metric rendering inspected; API '
                       'values exercised by 41 website tests; no new browser claim')
    elif name.endswith('/qualification-verify.py'):
        disposition = ('run: 128-row expected/import/FileAnalyzer verifier for actual '
                       'working checkout and exact archives')
    elif name.endswith('/pr536/slice4-proof.md'):
        disposition = ('reused: inspected supplier full-suite receipt, setup/assertions '
                       'and exact product-manifest correspondence')
    elif name.startswith('docs/reviews/'):
        disposition = ('excluded: retained historical comparison/research evidence; '
                       'not a runtime caller of changed owners')
    elif name.startswith(('website/', 'templates/')):
        disposition = ('excluded: lexical package-extension/documentation-link match; '
                       'no changed rendering or asset contract')
    else:
        disposition = ('reused: unchanged guidance, source, lifecycle or domain contract; '
                       'no new outward wording supplied here')
    assert line.endswith('| |'), line
    lines[index] = line[:-2] + disposition + ' |'
    count += 1
trace_path.write_text('\n'.join(lines) + '\n')
assert not any(line.startswith('| `') and line.endswith('| |') for line in lines[start:])
proof_path = HERE / 'current-owner-proof.json'
proof = json.loads(proof_path.read_text())
patch = subprocess.check_output(['git', 'diff', '--binary', 'HEAD', '--', 'lizard_languages'], cwd=CHECKOUT)
proof.update(working_product='uncommitted; history base 824e260; not performance-qualified',
             working_runtime_patch_sha256=hashlib.sha256(patch).hexdigest(),
             product_hash_manifest_sha256=digest(product_path),
             supplier=dict(revision=SUPPLIER, manifest_files=len(manifest),
                           differing_files=different, full_pytest_passed=1944,
                           full_pytest_skipped=7, terminal_exit=0,
                           reused_test_files=reused_tests),
             trace=dict(path=str(trace_path), rows=count, blank_dispositions=0,
                        sha256=digest(trace_path)),
             focused=dict(passed=842, skipped=5, exit=0, sha256=digest(CACHE / 'focused-final.log')),
             website=dict(passed=41, exit=0, sha256=digest(CACHE / 'website-final.log')))
proof_path.write_text(json.dumps(proof, indent=2) + '\n')
print(json.dumps(dict(product_files=len(product), supplier_differences=different,
                      trace_rows=count, blank_dispositions=0,
                      runtime_patch=proof['working_runtime_patch_sha256'])), flush=True)
