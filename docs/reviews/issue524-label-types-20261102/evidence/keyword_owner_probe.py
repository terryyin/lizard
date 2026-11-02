"""Observe contextual words at runtime operand and property boundaries."""

import argparse
import hashlib
import importlib
import json
from pathlib import Path
import sys


CASES = {
    'property_class': 'const result = receiver.class(() => 1);\n'
                      'const next = {run: () => 2};\n',
    'property_as': 'const result = receiver?.as(() => 1);\n'
                   'const next = {run: () => 2};\n',
    'operand_as': 'const result = flag ? as(() => 1) : value;\n',
    'operand_satisfies': 'const result = flag || satisfies(() => 1);\n',
    'assertion_control': 'const run = () => value as {x: () => number};\n',
}


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--checkout', type=Path, required=True)
    parser.add_argument('--out', type=Path, required=True)
    args = parser.parse_args()
    root = args.checkout.resolve()
    sys.path.insert(0, str(root))
    lizard = importlib.import_module('lizard')
    observations = []
    for case, statement in CASES.items():
        for suffix in ('js', 'ts', 'jsx', 'tsx', 'mjs', 'cjs', 'vue'):
            if case == 'assertion_control' and suffix not in ('ts', 'tsx', 'vue'):
                continue
            source = statement + 'function after(tail) {return tail;}\n'
            if suffix == 'vue':
                source = '<script lang="ts">' + source + '</script>'
            info = lizard.analyze_file.analyze_source_code('owner.' + suffix, source)
            observations.append({'case': case, 'suffix': suffix, 'source': source,
                                 'functions': [{key: getattr(f, key) for key in (
                                     'name', 'parameters', 'start_line', 'end_line',
                                     'cyclomatic_complexity', 'nloc', 'token_count')}
                                    for f in info.function_list]})
    modules = {}
    for name, module in list(sys.modules.items()):
        if name == 'lizard' or name.startswith(('lizard_core', 'lizard_languages',
                                               'lizard_ext')):
            filename = getattr(module, '__file__', None)
            if filename:
                path = Path(filename).resolve()
                assert root in path.parents
                modules[name] = {'path': str(path),
                                 'sha256': hashlib.sha256(path.read_bytes()).hexdigest()}
    with args.out.open('x') as output:
        json.dump({'checkout': str(root), 'python': sys.version, 'modules': modules,
                   'observations': observations}, output, indent=2)
        output.write('\n')
    print(json.dumps({'out': str(args.out), 'rows': len(observations),
                      'names': [{k: r[k] for k in ('case', 'suffix')} | {
                          'names': [f['name'] for f in r['functions']]}
                          for r in observations]}))


if __name__ == '__main__':
    main()
