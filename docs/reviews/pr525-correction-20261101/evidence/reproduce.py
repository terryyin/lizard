"""Replay PR525 promises and nearby public behavior against a frozen checkout."""

import argparse
import json
from pathlib import Path
import sys


ANON = ('(anonymous)', 1, 1, 1)
CASES = [
    ('map', "it('t', () => {\n  const m = new Map([\n"
     "    ['a', () => f(), { id: 1 }],\n  ]);\n  g(r);\n});\n",
     [('(anonymous)', 1, 3, 3), ('(anonymous)', 1, 1, 6)], False),
    ('call', 'h(() => f(), { id: 1 });\ng(r);\n', [ANON], False),
    ('nested-array', 'h([[() => f(), { id: 1 }]]);\ng(r);\n', [ANON], False),
    ('array-object', 'const a = [() => f(), { id: 1 }];\ng(r);\n', [ANON], False),
    ('array-name', 'const a = [() => f(), x];\ng(r);\n', [ANON], False),
    ('block-control', 'const a = [() => { f(); }, { id: 1 }];\ng(r);\n',
     [ANON], False),
    ('default', 'h((a = 1) => f(), { id: 1 });\ng(r);\n',
     [('(anonymous)', 2, 1, 1)], False),
    ('object-arrow', 'h(() => f(), { id: () => g() });\ng(r);\n',
     [ANON, ('id', 1, 1, 1)], False),
    ('subscript', "it('t', () => {\n  e(r[0]);\n  g(x);\n});\n",
     [('(anonymous)', 1, 1, 4)], False),
    ('subscript-chain', "it('t', () => {\n  e(r[0]).t([{}]);\n});\n",
     [('(anonymous)', 1, 1, 3)], False),
    ('object-array', 'function f() {\n  return { w: [m[n - 1]] };\n}\n'
     'function g() {}\n', [('f', 1, 1, 3), ('g', 1, 4, 4)], False),
    ('following-function', 'h(() => f(), { id: 1 });\n'
     'function after(x) { return x ? 1 : 0; }\n',
     [ANON, ('after', 2, 2, 2)], True),
    ('subscript-following', "it('t', () => {\n  e(r[0]);\n"
     '  if (x) g(x);\n});\nfunction after() {}\n',
     [('(anonymous)', 2, 1, 4), ('after', 1, 5, 5)], True),
    ('object-members', 'const o = { a: () => 1,\n  b: () => 2 };\n'
     'function after() {}\n', [('a', 1, 1, 1), ('b', 1, 2, 2),
                               ('after', 1, 3, 3)], True),
    ('label', 'const x = [() => { a: 1 }];\nfunction after() {}\n',
     [ANON, ('after', 1, 2, 2)], True),
    ('nested-object-array', 'function f() {\n  return { e: [ { w: [m[0]] } ], '
     'b: { [a]: 1 } };\n}\nfunction after() {}\n',
     [('f', 1, 1, 3), ('after', 1, 4, 4)], True),
    ('as-type', 'const t = () => x as { b: string };\nfunction after() {}\n',
     [('t', 1, 1, 1), ('after', 1, 2, 2)], True),
    ('class-type', 'class A {\n  private d: (() => void)[] = [];\n'
     '  constructor() { x(); }\n  s() { return 1; }\n}\nfunction after() {}\n',
     [('constructor', 1, 3, 3), ('s', 1, 4, 4), ('after', 1, 6, 6)], True),
    ('template', 'h(() => f(), { value: `${x}`, id: () => g() });\n'
     'function after() {}\n', [ANON, ('id', 1, 1, 1), ('after', 1, 2, 2)], True),
    ('typed-ternary', 'const fn = ok ? (x): number => x : (x): number => x;\n'
     'function after() {}\n', [('(anonymous)', 1, 1, 1), ('(anonymous)', 1, 1, 1),
                               ('after', 1, 2, 2)], True),
]


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--checkout', required=True)
    parser.add_argument('--out', required=True)
    parser.add_argument('--extension')
    args = parser.parse_args()
    sys.path.insert(0, str(Path(args.checkout).resolve()))
    import lizard
    analyzer = (lizard.FileAnalyzer(lizard.get_extensions([args.extension]))
                if args.extension else lizard.analyze_file)
    results = []
    for suffix in ('js', 'ts', 'jsx', 'tsx', 'mjs', 'cjs', 'vue'):
        for name, source, expected, control in CASES:
            if name in ('as-type', 'class-type', 'typed-ternary') and suffix in (
                    'js', 'jsx', 'mjs', 'cjs'):
                continue
            if suffix == 'vue':
                source = '<script lang="ts">' + source + '</script>'
            row = dict(case=name, suffix=suffix, source=source, control=control,
                       expected=expected)
            try:
                functions = analyzer.analyze_source_code(
                    'a.' + suffix, source).function_list
                actual = [(f.name, f.cyclomatic_complexity,
                           f.start_line, f.end_line) for f in functions]
                row.update(actual=actual, passes=actual == expected,
                           metrics=[dict(nloc=f.nloc, tokens=f.token_count,
                                         parameters=f.full_parameters)
                                    for f in functions])
            except Exception as error:  # Retain public crashes as review evidence.
                row.update(error=type(error).__name__ + ': ' + str(error),
                           passes=False)
            results.append(row)
    summary = dict(checkout=str(Path(args.checkout).resolve()),
                   imported=lizard.__file__, extension=args.extension,
                   total=len(results),
                   passed=sum(r['passes'] for r in results),
                   authored_total=sum(not r['control'] for r in results),
                   authored_passed=sum(r['passes'] and not r['control']
                                       for r in results),
                   crashes=sum('error' in r for r in results))
    Path(args.out).write_text(json.dumps(dict(summary=summary, results=results),
                                         indent=2) + '\n')
    print(json.dumps(summary))
    for row in results:
        if row['suffix'] in ('js', 'ts') and not row['passes']:
            print(row['case'], row['suffix'],
                  row.get('error', row.get('actual')))


if __name__ == '__main__':
    main()
