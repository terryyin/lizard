"""Extend qualification coverage without mutating the profiled input sets."""

import argparse
import hashlib
import json
from pathlib import Path
import shutil


def hashes(root):
    return {str(p.relative_to(root)): hashlib.sha256(p.read_bytes()).hexdigest()
            for p in sorted(root.rglob('*')) if p.is_file()}


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--cache', required=True, type=Path)
    args = parser.parse_args()
    cache = args.cache.resolve()
    evidence = Path(__file__).resolve().parent
    original = json.loads((evidence / 'workload-manifest.json').read_text())
    for name, expected in original['inputs'].items():
        assert hashes(cache / name) == expected, name
    complete, cumulative = cache / 'complete', cache / 'complete-parent-safe'
    assert not complete.exists() and not cumulative.exists()
    shutil.copytree(cache / 'affected', complete)
    shutil.copytree(cache / 'parent-safe', cumulative)
    for suffix in ('js', 'ts', 'tsx', 'vue'):
        for callee in ('h', 'api.h', 'new Factory', 'api?.h', 'api?.'):
            for in_object in (False, True):
                stem = 'argument-%s-%s-%s' % (
                    callee.replace('?', 'optional').replace('.', '-'),
                    'object' if in_object else 'statement', suffix)
                call = callee + '(() => f(), {id: (x = 1) => x || 0})'
                safe_call = callee + '(() => { f(); }, marker)'
                for target, expression in ((complete, call),
                                            (cumulative, safe_call)):
                    source = (('const o = {value: ' + expression + '}'
                               if in_object else expression) + ';\n'
                              'function after(tail) { return tail ? 1 : 0; }\n')
                    if suffix == 'vue':
                        source = '<script lang="ts">' + source + '</script>'
                    (target / (stem + '.' + suffix)).write_text(source)
    manifest = dict(
        reason='Additional known-call/object argument consumers from semantic '
               'and cost traces, selected before the first paired measurement. '
               'Profiled affected/parent-safe/broad sets remain unchanged.',
        profiled_manifest_sha256=hashlib.sha256(
            (evidence / 'workload-manifest.json').read_bytes()).hexdigest(),
        inputs={name: hashes(cache / name) for name in (
            'complete', 'complete-parent-safe', 'broad')})
    (evidence / 'qualification-inputs.json').write_text(
        json.dumps(manifest, indent=2) + '\n')
    print(json.dumps({name: len(files)
                      for name, files in manifest['inputs'].items()}))


if __name__ == '__main__':
    main()
