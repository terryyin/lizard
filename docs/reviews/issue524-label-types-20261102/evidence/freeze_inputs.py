"""Freeze issue524 qualification inputs before paired timing."""

import argparse
import hashlib
import io
import json
from pathlib import Path
import shutil
import subprocess
import tarfile


PRIOR = 'f5d10b7c371b933947d15805f484b33c1b39b0cb'
PRIOR_EVIDENCE = 'docs/reviews/pr525-correction-20261101/evidence/'
BASELINES = {
    'current': 'da29d5dfd904ba7333199a79ffbf4c0da24c5641',
    'cumulative': '6d288d4cdcb85bc50ab4c8a887196177607eeb7d',
}


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def files(root):
    return {str(p.relative_to(root)): digest(p)
            for p in sorted(root.rglob('*')) if p.is_file()}


def prior_json(filename):
    return json.loads(subprocess.check_output(
        ['git', 'show', PRIOR + ':' + PRIOR_EVIDENCE + filename], text=True))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--cache', type=Path, required=True)
    parser.add_argument('--prior-cache', type=Path, required=True)
    parser.add_argument('--cases', type=Path, required=True)
    parser.add_argument('--out', type=Path, required=True)
    parser.add_argument('--call-pairs', type=int, default=20)
    args = parser.parse_args()
    cache = args.cache.resolve()
    prior = args.prior_cache.resolve()
    qualification = prior_json('qualification-inputs.json')
    arguments = prior_json('argument-inputs.json')
    for name in ('complete', 'broad'):
        assert files(prior / name) == qualification['inputs'][name], name
    assert files(prior / 'argument-consumers') == arguments['inputs'][
        'argument-consumers']
    cases = json.loads(args.cases.read_text())
    assert cases and {row['family'] for row in cases} == {'labels', 'annotations'}
    cache.mkdir(parents=True, exist_ok=False)
    roots = {}
    for label, revision in BASELINES.items():
        target = cache / label
        target.mkdir()
        archive = subprocess.check_output([
            'git', 'archive', revision, 'lizard.py', 'lizard_core',
            'lizard_languages', 'lizard_ext'])
        archive_path = cache / (label + '.tar')
        archive_path.write_bytes(archive)
        with tarfile.open(fileobj=io.BytesIO(archive)) as source:
            assert all(not Path(m.name).is_absolute()
                       and '..' not in Path(m.name).parts
                       and (m.isfile() or m.isdir()) for m in source.getmembers())
            source.extractall(target)
        roots[label] = {'revision': revision, 'archive': str(archive_path),
                        'archive_sha256': digest(archive_path),
                        'files': files(target)}
    for old_name, new_name in (('complete', 'complete'), ('broad', 'broad'),
                                ('argument-consumers', 'calls')):
        shutil.copytree(prior / old_name, cache / new_name)
    for family in ('labels', 'annotations'):
        (cache / family).mkdir()
    for row in cases:
        family, name = row['family'], row['filename']
        assert family in ('labels', 'annotations') and Path(name).name == name
        target = cache / family / name
        assert not target.exists(), str(target)
        target.write_text(row['source'])
        combined = cache / 'complete' / ('issue524-' + family + '-' + name)
        assert not combined.exists(), str(combined)
        combined.write_text(row['source'])
    record = {
        'baseline_roots': roots, 'prior_revision': PRIOR,
        'prior_cache': str(prior), 'cases_sha256': digest(args.cases),
        'cases': cases,
        'inputs': {name: files(cache / name) for name in (
            'labels', 'annotations', 'calls', 'complete', 'broad')},
        'protocol': {'pairs': 20, 'call_pairs': args.call_pairs,
                     'api_calibration_seconds': 1.2,
                     'cli_calibration_seconds': 4, 'minimum_batch_seconds': 1,
                     'strict_paired_95pct_upper_limit': 3},
        'selection': 'Actual regression/preservation fixtures and unchanged '
                     'prior complete/call/broad sets; frozen before timing. '
                     'New families are qualified independently.'}
    with args.out.open('x') as stream:
        json.dump(record, stream, indent=2)
        stream.write('\n')
    print(json.dumps({name: len(paths)
                      for name, paths in record['inputs'].items()}))


if __name__ == '__main__':
    main()
