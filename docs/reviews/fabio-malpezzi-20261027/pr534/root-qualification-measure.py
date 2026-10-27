"""Run each prospectively fixed exact-revision gate once, retaining child exits."""

import json
import sys

from root_qualification_common import (
    CACHE, GATES, REVISIONS, assert_inputs, digest, host_observation,
    now, observations, run_logged, write_json)


def main():
    assert_inputs()
    assert not (CACHE / 'decision.json').exists(), 'Never silently repeat qualification'
    observation = host_observation()
    assert not observation['competing_commands'], observation['competing_commands']
    decision = dict(
        observed=observation, owner='qualification9_root',
        workload='six serial 20-pair warmed comparisons, 1.5-second calibration',
        resource_bound='one active timed analyzer/CLI process; API has two idle/active workers',
        expected_time='about seven to ten minutes plus interpreter/process setup',
        choice='run serially on16CPU/48GiB; observed load and available memory support bounded one-core work',
        owned_competition='none; parent/root read-only, ordinary CI observer remains active',
        unknowns='other native worktrees/VM activity may continue outside our ownership',
        reconsider='new contention/failure or unexplained noise; no favorable retry',
        fixed_before_timing=True, gates=GATES)
    write_json(CACHE / 'decision.json', decision)
    print(json.dumps(dict(start='six fixed comparisons', host=observation['host'],
                          load=observation['load'])), flush=True)
    summary = {}
    for label, corpus, baseline, workload in GATES:
        runner = CACHE / corpus
        destination = CACHE / (label + '.json')
        assert not destination.exists(), 'Never overwrite a result'
        args = [sys.executable, str(runner / 'paired.py'),
                '--baseline', str(CACHE / baseline), '--candidate', str(CACHE / 'candidate'),
                '--workload', workload, '--pairs', '20', '--seconds', '1.5',
                '--out', str(destination)]
        print(json.dumps(dict(start=label)), flush=True)
        receipt = run_logged(args, label + '-terminal', cwd=runner / 'corpus')
        data = json.loads(destination.read_text())
        assert len(data['raw']) == 20
        minimum = min(value for pair in data['raw'] for value in pair.values())
        assert minimum > 1, (label, minimum)
        assert data['paths'] == [str(CACHE / baseline), str(CACHE / 'candidate')]
        assert data['passes'] == (data['paired_95pct_ci'][1] < 3)
        summary[label] = dict(
            median_slowdown_pct=data['median_slowdown_pct'],
            paired_geometric_slowdown_pct=data['paired_geometric_slowdown_pct'],
            paired_95pct_ci=data['paired_95pct_ci'], passes=data['passes'],
            minimum_batch_seconds=minimum, pairs=20, child_returncode=receipt['returncode'],
            result_sha256=digest(destination))
        print(json.dumps(dict(finished=label, **summary[label])), flush=True)
    assert_inputs()
    public = observations('after')
    assert_inputs()
    before = json.loads((CACHE / 'before-public-metrics.json').read_text())
    assert before == json.loads((CACHE / 'after-public-metrics.json').read_text())
    result = dict(finished=now(), revisions=REVISIONS, gates=summary,
                  all_pass=all(row['passes'] for row in summary.values()),
                  raw_runs_complete=True, public_before_after_equal=True,
                  post_observation_receipts=public, driver_returncode=0)
    write_json(CACHE / 'summary.json', result)
    write_json(CACHE / 'measure-driver-receipt.json', dict(finished=now(), returncode=0))
    print(json.dumps(dict(qualification_complete=result['all_pass'],
                          all_six_commands_terminal=True, driver_returncode=0)), flush=True)


if __name__ == '__main__':
    main()
