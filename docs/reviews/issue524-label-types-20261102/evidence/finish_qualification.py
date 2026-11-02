"""Extend borderline NS precision once, retaining every original pair."""

import argparse
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys


def read(path):
    return json.loads(path.read_text())


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--cache', type=Path, required=True)
    parser.add_argument('--evidence', type=Path, required=True)
    parser.add_argument('--out', type=Path, required=True)
    parser.add_argument('--input-manifest', default='qualification-inputs.json')
    parser.add_argument('--candidate-record', default='candidate2.json')
    parser.add_argument('--previous-run', default='performance2')
    args = parser.parse_args()
    cache, evidence, output = args.cache.resolve(), args.evidence.resolve(), args.out.resolve()
    runner = Path.cwd() / '.agents/skills/lizard-process-pr/scripts/paired_benchmark.py'
    spec = importlib.util.spec_from_file_location('official_paired', runner)
    paired = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(paired)
    frozen = read(evidence / args.input_manifest)
    candidate = read(evidence / args.candidate_record)['runtime']
    previous = evidence / args.previous_run
    assert paired.source_state(Path.cwd()) == candidate
    for family, expected in frozen['inputs'].items():
        assert paired.corpus_state(cache / family) == expected
    for label, root in frozen['baseline_roots'].items():
        assert paired.source_state(cache / label) == {
            p: digest for p, digest in root['files'].items() if p.endswith('.py')}

    def verify(report, baseline, family, workload, extension):
        assert report['workload'] == workload and report['extension'] == extension
        assert report['sources'][0]['files'] == paired.source_state(cache / baseline)
        assert report['sources'][1]['files'] == candidate
        assert report['corpus']['files'] == frozen['inputs'][family]
        assert report['python'] == sys.version and report['executable'] == sys.executable
        for path, digest in report['harness'].items():
            assert hashlib.sha256(Path(path).read_bytes()).hexdigest() == digest
        assert all(row['stable'] for row in report['stability'])
        assert all(a['observation_sha256'] == b['observation_sha256']
                   for a, b in zip(report['workers'], report['stability']))
        assert min(min(pair.values()) for pair in report['raw']) >= 1

    output.mkdir(parents=True, exist_ok=False)
    original_path = previous / 'current-calls-ns.json'
    original = read(original_path)
    verify(original, 'current', 'calls', 'api-parameters', 'NS')
    assert original['verdict'] == 'bound-not-met'
    assert len(original['raw']) == frozen['protocol'].get('call_pairs', 20)
    total_pairs = len(original['raw']) + 80
    decision = {
        'load': os.getloadavg(), 'host': os.uname().nodename,
        'decision': ('One prospective precision extension, no product/input change. '
                     f'Original NS mean{original["paired_change_pct"]:.6f}%, '
                     f'upper{original["paired_95pct_ci"][1]:.6f}% is retained. '
                     'Exact NS profiles show unchanged extension call counts and '
                     'no new extension hotspot. Add80 four-second alternating '
                     f'pairs; evaluate all{total_pairs} with the installed '
                     'conservative t(19) method. Require both the80-pair block '
                     f'and pooled{total_pairs} bound strictly below3%. If either '
                     'fails, stop for correction; no further precision retry.'),
        'runtime': candidate, 'additional_pairs': 80, 'additional_seconds': 4,
        'original': str(original_path)}
    (output / 'decision.json').write_text(json.dumps(decision, indent=2) + '\n')
    summary = []
    for family in ('calls', 'labels', 'annotations', 'complete'):
        path = previous / ('current-' + family + '-api-parameters.json')
        report = read(path)
        verify(report, 'current', family, 'api-parameters', None)
        assert report['passes']
        summary.append({'label': 'current-' + family + '-api-parameters',
                        'report': str(path), 'reused': True,
                        'paired_95pct_ci': report['paired_95pct_ci'], 'passes': True})

    def measure(baseline, family, workload, extension, pairs=20, seconds=1.2, extra=''):
        label = '-'.join((baseline, family, 'ns' if extension else workload)) + extra
        path = output / (label + '.json')
        command = [sys.executable, str(runner), '--baseline', str(cache / baseline),
                   '--candidate', str(Path.cwd()), '--corpus', str(cache / family),
                   '--workload', workload, '--out', str(path), '--pairs', str(pairs),
                   '--seconds', str(seconds), '--limit-pct', '3']
        if extension:
            command.extend(['--extension', extension])
        print('Starting ' + label, flush=True)
        result = subprocess.run(command, capture_output=True, text=True)
        (output / (label + '-command.json')).write_text(json.dumps({
            'command': command, 'exit': result.returncode, 'stdout': result.stdout,
            'stderr': result.stderr}, indent=2) + '\n')
        report = read(path)
        verify(report, baseline, family, workload, extension)
        return label, path, report

    label, path, extra = measure('current', 'calls', 'api-parameters', 'NS', 80, 4, '-extra')
    assert [w['observation_sha256'] for w in original['workers']] == [
        w['observation_sha256'] for w in extra['workers']]
    pooled = paired.evaluate(original['raw'] + extra['raw'], 3)
    pooled.update({'original_report': decision['original'], 'additional_report': str(path),
                   'raw': original['raw'] + extra['raw'], 'all_pairs_retained': True,
                   'additional_block_passes': extra['passes'],
                   'accepted': pooled['passes'] and extra['passes']})
    pooled_path = output / 'current-calls-ns-pooled.json'
    pooled_path.write_text(json.dumps(pooled, indent=2) + '\n')
    print(json.dumps({'label': 'current-calls-ns-pooled', 'passes': pooled['accepted'],
                      'paired_95pct_ci': pooled['paired_95pct_ci']}), flush=True)
    summary.append({'label': 'current-calls-ns', 'report': str(pooled_path),
                    'paired_95pct_ci': pooled['paired_95pct_ci'], 'passes': pooled['accepted']})
    (output / 'summary.json').write_text(json.dumps(summary, indent=2) + '\n')
    if not pooled['accepted']:
        return 1
    remaining = [('current', f, 'api-parameters', 'NS')
                 for f in ('labels', 'annotations', 'complete')]
    remaining.append(('current', 'broad', 'cli', None))
    remaining.extend(('cumulative', f, 'api-parameters', ext)
                     for ext in (None, 'NS')
                     for f in ('calls', 'labels', 'annotations', 'complete'))
    remaining.append(('cumulative', 'broad', 'cli', None))
    for baseline, family, workload, extension in remaining:
        label, path, report = measure(baseline, family, workload, extension,
                                     pairs=(frozen['protocol'].get('call_pairs', 20)
                                            if family == 'calls' else 20),
                                     seconds=4 if workload == 'cli' else 1.2)
        row = {'label': label, 'report': str(path), 'passes': report['passes'],
               'paired_95pct_ci': report['paired_95pct_ci']}
        summary.append(row)
        (output / 'summary.json').write_text(json.dumps(summary, indent=2) + '\n')
        print(json.dumps(row), flush=True)
        if not report['passes']:
            return 1
    assert len(summary) == 18 and all(row['passes'] for row in summary)
    assert paired.source_state(Path.cwd()) == candidate
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
