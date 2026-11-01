"""Freeze the PR525 workloads before any qualified timing is attempted."""

import argparse
import hashlib
import importlib.util
import json
from pathlib import Path
import shutil
import tarfile


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--cache', required=True, type=Path)
    args = parser.parse_args()
    cache = args.cache.resolve()
    cache.mkdir(exist_ok=False)
    evidence = Path(__file__).resolve().parent
    roots = {}
    for label, revision, archive in (
            ('current', '6d288d4cdcb85bc50ab4c8a887196177607eeb7d',
             '/Users/terryyin/.cache/pr525-current-6d288d4.tar'),
            ('parent', '01afe9c228ba2a3dc5253259d8415d2ef47f0b71',
             '/Users/terryyin/.cache/pr525-parent-01afe9c.tar')):
        target = cache / label
        target.mkdir()
        with tarfile.open(archive) as source:
            source.extractall(target)
        roots[label] = dict(revision=revision, archive=archive,
                            archive_sha256=digest(Path(archive)),
                            files={str(p.relative_to(target)): digest(p)
                                   for p in sorted(target.rglob('*.py'))})
    spec = importlib.util.spec_from_file_location(
        'replay_inputs', evidence / 'reproduce.py')
    replay = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(replay)
    affected, parent_safe, broad = [cache / name for name in (
        'affected', 'parent-safe', 'broad')]
    for target in (affected, parent_safe, broad):
        target.mkdir()
    safe_names = {'subscript', 'subscript-chain', 'object-array', 'array-name',
                  'block-control', 'template'}
    for name, source, _, _ in replay.CASES:
        for suffix in ('js', 'ts', 'jsx', 'tsx', 'mjs', 'cjs', 'vue'):
            if name in ('as-type', 'class-type', 'typed-ternary') and suffix in (
                    'js', 'jsx', 'mjs', 'cjs'):
                continue
            text = ('<script lang="ts">' + source + '</script>'
                    if suffix == 'vue' else source)
            filename = name + '.' + suffix
            (affected / filename).write_text(text)
            if name in safe_names:
                (parent_safe / filename).write_text(text)
    extras = {
        'generic-call.ts': 'const f = (x) => foo<A,\n B>(x);\n'
                           'function after(y) { return y || 0; }\n',
        'typed-default.ts': 'const f = (x: number = 1): number => x || 0;\n'
                            'function after(y) { return y; }\n',
        'plain-control.js': 'function outer(x) {\n'
                            ' if (x) return x.map(v => v || 0);\n'
                            ' return [];\n}\nfunction after(y) { return y; }\n',
    }
    for name, source in extras.items():
        (affected / name).write_text(source)
        (parent_safe / name).write_text(source)
    for directory in ('lizard_core', 'lizard_languages', 'lizard_ext'):
        shutil.copytree(cache / 'parent' / directory, broad / directory)
    shutil.copyfile(cache / 'parent' / 'lizard.py', broad / 'lizard.py')
    manifest = dict(roots=roots, inputs={})
    for target in (affected, parent_safe, broad):
        manifest['inputs'][target.name] = {
            str(p.relative_to(target)): digest(p)
            for p in sorted(target.rglob('*')) if p.is_file()}
    (evidence / 'workload-manifest.json').write_text(
        json.dumps(manifest, indent=2) + '\n')
    print(json.dumps({name: len(files)
                      for name, files in manifest['inputs'].items()}))


if __name__ == '__main__':
    main()
