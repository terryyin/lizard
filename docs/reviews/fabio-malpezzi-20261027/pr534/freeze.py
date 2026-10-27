"""Freeze PR534 examples, colon controls, and inherited PR531 boundaries."""
import hashlib
import json
from pathlib import Path
import shutil
import subprocess

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
CACHE = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027')
DEST = CACHE / 'pr534'
BASE = '01afe9c228ba2a3dc5253259d8415d2ef47f0b71'
HEAD = 'f4074ae860c7999cc37dd081a6f7ebfa777adc83'

def git(*args):
    return subprocess.check_output(['git', '-C', str(ROOT), *args])

DEST.mkdir(exist_ok=True)
patch = git('diff', BASE, HEAD)
(HERE / 'proposed-patch.json').write_text(json.dumps(dict(
    base=BASE, head=HEAD, patch=patch.decode())) + '\n')
for name in ('target', 'raw', 'child'):
    path = DEST / name
    path.mkdir(exist_ok=True)
    archive = git('archive', '7186fdbe826bf32b399165cc3d36897a128d4d0d')
    subprocess.run(['tar', '-xf', '-', '-C', str(path)], input=archive, check=True)
    if name != 'target':
        subprocess.run(['git', 'apply', '-'], input=patch, cwd=path, check=True)
child = DEST / 'child/lizard_languages/typescript_states.py'
text = child.read_text().replace(
    'self.started_function and not self._opens_an_object()', 'self.started_function')
start = text.index('    def _opens_an_object(self):')
end = text.index('    def _expecting_condition_and_statement_block', start)
child.write_text(text[:start] + text[end:])
cases = []

def add(label, source, expected, extensions=('js', 'ts', 'jsx', 'tsx')):
    for ext in extensions:
        cases.append(dict(label=label, filename=label + '.' + ext,
                          source=source, expected=expected))

choose = ('function choose(strict, b) {\n'
          '  const eq = strict ? (x, y) => x === y : (x, y) => x == y || b;\n'
          '  return eq;\n}\n')
add('reported', choose, [['(anonymous)', 2, 2, 1],
                        ['(anonymous)', 2, 2, 2], ['choose', 1, 4, 2]])
authored = ('function f(a, b) {\n'
            '  const g = a ? (x) => x : (y) => y && b;\n'
            '  const h = a ? (x) => x ? 1 : 2 : null;\n'
            '  return g;\n}\n')
add('authored', authored, [['(anonymous)', 2, 2, 1], ['(anonymous)', 2, 2, 2],
                         ['(anonymous)', 3, 3, 2], ['f', 1, 5, 3]])

def wrapper(label, statement, arrows, outer=2, extensions=('js', 'ts', 'jsx', 'tsx')):
    source = 'function outer(c, d) {\n  ' + statement + '\n  return 1;\n}\n' \
             'function after() { return 1; }\n'
    add(label, source, arrows + [['outer', 1, 4, outer], ['after', 5, 5, 1]],
        extensions)

wrapper('internal_ternary', 'const f = x => c ? x : d;', [['f', 2, 2, 2]], 1)
wrapper('nested_ternary', 'const f = c ? d ? x => x : y => y && d : z => z || c;',
        [['f', 2, 2, 1], ['(anonymous)', 2, 2, 2],
         ['(anonymous)', 2, 2, 2]], 3)
wrapper('nested_arrows', 'const f = c ? x => y => y && x : z => z || c;',
        [['f', 2, 2, 2], ['(anonymous)', 2, 2, 2]])
wrapper('nested_arrow_internal', 'const f = c ? x => d ? y => y : z => z || x : w => w;',
        [['f', 2, 2, 3], ['(anonymous)', 2, 2, 1]])
wrapper('grouped_nested_arrow', 'const f = c ? x => (y => y && x) : z => z || c;',
        [['(anonymous)', 2, 2, 2], ['f', 2, 2, 1],
         ['(anonymous)', 2, 2, 2]])
wrapper('branch_object', 'const f = c ? x => ({a: x || d}) : y => y && d;',
        [['f', 2, 2, 2], ['(anonymous)', 2, 2, 2]])
wrapper('object_property', 'const o = {f: c ? x => x : y => y || d, next: z => z && c};',
        [['f', 2, 2, 1], ['(anonymous)', 2, 2, 2], ['next', 2, 2, 2]])
wrapper('computed_key', 'const o = {[c ? "a" : "b"]: x => x || d, next: y => y && c};',
        [['c?a:b', 2, 2, 2], ['next', 2, 2, 2]])
wrapper('object_inside_ternary', 'const f = c ? x => d ? {a: x} : {b: x} : y => y || d;',
        [['f', 2, 2, 2], ['(anonymous)', 2, 2, 2]])
wrapper('typed_arrows', 'const f = c ? (x: number): number => x : (y: number): number => y || d;',
        [['(anonymous)', 2, 2, 1], ['(anonymous)', 2, 2, 2]], extensions=('ts', 'tsx'))
wrapper('type_assertion', 'const f = c ? (x: number) => x as number : (y: number) => y || d;',
        [['(anonymous)', 2, 2, 1], ['(anonymous)', 2, 2, 2]], extensions=('ts', 'tsx'))
wrapper('typed_object_return', 'const f = c ? (x: number): {a: number} => ({a: x}) : y => y || d;',
        [['(anonymous)', 2, 2, 1], ['(anonymous)', 2, 2, 2]], extensions=('ts', 'tsx'))
wrapper('block_branches', 'const f = c ? x => { return x; } : y => { return y || d; };',
        [['f', 2, 2, 1], ['(anonymous)', 2, 2, 2]])
wrapper('plain_ternary', 'const f = c ? d : c || d;', [], 3)
for label, source, expected in (
    ('jsx_attribute', 'function view(c, d) {\n return <Panel f={c ? x => x : y => y || d}/>;\n}\n',
     [['(anonymous)', 2, 2, 1], ['(anonymous)', 2, 2, 2], ['view', 1, 3, 2]]),):
    add(label, source, expected, ('jsx', 'tsx'))
inherited = json.loads((HERE.parent / 'pr531/inputs.json').read_text())
for case in inherited:
    if case['label'] in ('reported', 'author_test', 'function_type_string',
                         'function_type_number', 'function_type_union', 'function_type_template',
                         'function_type_boolean', 'in_operator', 'block_body', 'group_object'):
        case = dict(case, label='inherited_' + case['label'],
                    filename='inherited_' + case['filename'])
        cases.append(case)
for label in ('reported', 'nested_ternary', 'nested_arrows', 'branch_object',
              'typed_arrows', 'inherited_function_type_string'):
    case = next(c for c in cases if c['label'] == label and c['filename'].endswith('.ts'))
    shifted = [[r[0], r[1] + 1, r[2] + 1, r[3]] for r in case['expected']]
    add('vue_' + label, '<script lang="ts">\n' + case['source'] + '</script>\n',
        shifted, ('vue',))
(HERE / 'inputs.json').write_text('[\n' + ',\n'.join(json.dumps(c) for c in cases) + '\n]\n')
corpus = DEST / 'corpus'
shutil.copytree(CACHE / 'corpus', corpus, ignore=shutil.ignore_patterns('affected'), dirs_exist_ok=True)
(corpus / 'affected').mkdir(exist_ok=True)
for case in cases:
    (corpus / 'affected' / case['filename']).write_text(case['source'])
historical_harness = subprocess.check_output([
    'git', 'show', 'af27c910a6a0bff51d8358fde0c37fa9cb059d1a:'
    'docs/reviews/fabio-malpezzi-20261027/paired.py'], cwd=HERE)
assert hashlib.sha256(historical_harness).hexdigest() == (
    '3064d8d77047db2f084ce9fd6ae09d98acf34453316b7df20fb184cbb3217bf5')
(DEST / 'paired.py').write_bytes(historical_harness)
shutil.copy2(HERE / 'inputs.json', DEST / 'inputs.json')
manifest = {str(p.relative_to(corpus)): hashlib.sha256(p.read_bytes()).hexdigest()
            for p in sorted(corpus.rglob('*')) if p.is_file()}
payload = dict(head=HEAD, base=BASE, target='7186fdbe826bf32b399165cc3d36897a128d4d0d',
               files=manifest, cases=len(cases),
               harness_sha256=hashlib.sha256((DEST / 'paired.py').read_bytes()).hexdigest())
for path in (DEST / 'manifest.json', HERE / 'manifest.json'):
    path.write_text(json.dumps(payload, indent=2) + '\n')
authored_test = (DEST / 'raw/test/test_languages/testES6.py').read_text()
authored_test = 'import unittest\nfrom lizard import analyze_file\n\n' + authored_test[
    authored_test.index('class Test_ES6_arrow_function_in_a_branch_of_a_ternary'):]
(HERE / 'authored-tests.py').write_text(authored_test)
print(json.dumps(dict(root=str(DEST), cases=len(cases), files=len(manifest))))
