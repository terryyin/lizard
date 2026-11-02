"""Run the frozen issue524 matrix serially through the installed paired runner."""

import argparse
import hashlib
import json
from pathlib import Path
import subprocess
import sys


def files(root):
    return {str(p.relative_to(root)): hashlib.sha256(p.read_bytes()).hexdigest()
            for p in sorted(root.rglob('*')) if p.is_file()
            and '__pycache__' not in p.parts}


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--cache', type=Path, required=True)
    parser.add_argument('--manifest', type=Path, required=True)
    parser.add_argument('--out', type=Path, required=True)
    parser.add_argument('--calls-first', action='store_true')
    parser.add_argument('--call-pairs', type=int, default=20)
    args = parser.parse_args()
    cache, output = args.cache.resolve(), args.out.resolve()
    manifest = json.loads(args.manifest.read_text())
    assert args.call_pairs == manifest['protocol'].get('call_pairs', 20)
    for name, expected in manifest['inputs'].items():
        assert files(cache / name) == expected, name
    for name, root in manifest['baseline_roots'].items():
        assert files(cache / name) == root['files'], name
        assert hashlib.sha256(Path(root['archive']).read_bytes()).hexdigest() == (
            root['archive_sha256'])
    output.mkdir(parents=True, exist_ok=False)
    runner = Path.cwd() / '.agents/skills/lizard-process-pr/scripts/paired_benchmark.py'
    summary = []
    # Choose order before launch. A diagnosed failed family can run first after
    # correction; frozen inputs, protocol and every required gate stay the same.
    families = (('calls', 'labels', 'annotations', 'complete') if args.calls_first
                else ('labels', 'annotations', 'calls', 'complete'))
    workloads = [(family, 'api-parameters', extension, '1.2')
                 for extension in (None, 'NS')
                 for family in families]
    workloads.append(('broad', 'cli', None, '4'))
    for baseline in ('current', 'cumulative'):
        for family, workload, extension, seconds in workloads:
            label = '-'.join((baseline, family, 'ns' if extension else workload))
            target = output / (label + '.json')
            command = [sys.executable, str(runner), '--baseline',
                       str(cache / baseline), '--candidate', str(Path.cwd()),
                       '--corpus', str(cache / family), '--workload', workload,
                       '--out', str(target), '--pairs',
                       str(args.call_pairs if family == 'calls' else 20),
                       '--seconds', seconds,
                       '--limit-pct', '3']
            if extension:
                command.extend(['--extension', extension])
            print('Starting ' + label, flush=True)
            result = subprocess.run(command, capture_output=True, text=True)
            report = json.loads(target.read_text()) if target.exists() else {}
            row = {'label': label, 'command': command, 'exit': result.returncode,
                   'stdout': result.stdout, 'stderr': result.stderr}
            for key in ('passes', 'verdict', 'paired_change_pct', 'paired_95pct_ci'):
                row[key] = report.get(key)
            summary.append(row)
            (output / 'summary.json').write_text(json.dumps(summary, indent=2) + '\n')
            print(json.dumps({key: row[key] for key in (
                'label', 'exit', 'verdict', 'paired_change_pct', 'paired_95pct_ci')}),
                flush=True)
            if result.returncode or not report.get('passes'):
                return 1
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
