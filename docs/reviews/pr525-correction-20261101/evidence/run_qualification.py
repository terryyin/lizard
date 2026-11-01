"""Run the frozen paired qualification matrix serially, preserving failures."""

import argparse
import hashlib
import json
from pathlib import Path
import subprocess
import sys


def hashes(root):
    return {str(p.relative_to(root)): hashlib.sha256(p.read_bytes()).hexdigest()
            for p in sorted(root.rglob('*')) if p.is_file()}


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--cache', required=True, type=Path)
    parser.add_argument('--out', required=True, type=Path)
    args = parser.parse_args()
    cache, output = args.cache.resolve(), args.out.resolve()
    evidence = Path(__file__).resolve().parent
    original = json.loads((evidence / 'workload-manifest.json').read_text())
    complete = json.loads((evidence / 'qualification-inputs.json').read_text())
    arguments = json.loads((evidence / 'argument-inputs.json').read_text())
    for manifest in (original, complete, arguments):
        for name, expected in manifest['inputs'].items():
            assert hashes(cache / name) == expected, name
    output.mkdir(parents=True, exist_ok=False)
    runner = Path.cwd() / '.agents/skills/lizard-process-pr/scripts/paired_benchmark.py'
    comparisons = (
        ('current-api', 'current', 'complete', 'api-parameters', None, '1.2'),
        ('current-ns', 'current', 'complete', 'api-parameters', 'NS', '1.2'),
        ('arguments-api', 'current', 'argument-consumers', 'api-parameters', None, '1.2'),
        ('arguments-ns', 'current', 'argument-consumers', 'api-parameters', 'NS', '1.2'),
        ('current-cli', 'current', 'broad', 'cli', None, '4'),
        ('parent-api', 'parent', 'complete-parent-safe', 'api-parameters', None, '1.2'),
        ('parent-ns', 'parent', 'complete-parent-safe', 'api-parameters', 'NS', '1.2'),
        ('parent-cli', 'parent', 'broad', 'cli', None, '4'),
    )
    summary = []
    for label, baseline, corpus, workload, extension, seconds in comparisons:
        command = [sys.executable, str(runner), '--baseline', str(cache / baseline),
                   '--candidate', str(Path.cwd()), '--corpus', str(cache / corpus),
                   '--workload', workload, '--out', str(output / (label + '.json')),
                   '--pairs', '20', '--seconds', seconds, '--limit-pct', '3']
        if extension:
            command.extend(['--extension', extension])
        print('Starting ' + label, flush=True)
        result = subprocess.run(command, capture_output=True, text=True)
        report = json.loads((output / (label + '.json')).read_text())
        row = dict(label=label, command=command, exit=result.returncode,
                   verdict=report.get('verdict'), passes=report.get('passes'),
                   paired_change_pct=report.get('paired_change_pct'),
                   paired_95pct_ci=report.get('paired_95pct_ci'),
                   stdout=result.stdout, stderr=result.stderr)
        summary.append(row)
        (output / 'summary.json').write_text(json.dumps(summary, indent=2) + '\n')
        print(json.dumps({k: row[k] for k in (
            'label', 'exit', 'verdict', 'paired_change_pct', 'paired_95pct_ci')}),
            flush=True)
        if result.returncode or not report.get('passes'):
            return 1
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
