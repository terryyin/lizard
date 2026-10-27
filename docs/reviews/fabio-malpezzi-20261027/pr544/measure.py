"""Incremental and cumulative paired workloads for PR544."""
import hashlib
import json
from pathlib import Path
import shutil
import subprocess
import sys

REPO = Path(__file__).resolve().parents[4]
EVIDENCE = Path(__file__).resolve().parent
CACHE = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027')
ROOT = CACHE / 'pr544'
PROOF = ROOT / 'proof'
PAIRED = PROOF / 'paired.py'
assert hashlib.sha256(PAIRED.read_bytes()).hexdigest() == (
    'ec1c9c5eac0ad6a29294ecbd987b2a55d4cf31db145486d065bf13c096b76520')

# api-parameters uses the broader consumers corpus by swapping affected.
consumers_dir = PROOF / 'corpus' / 'consumers'
affected_dir = PROOF / 'corpus' / 'affected'
affected_backup = PROOF / 'corpus' / 'affected-primary'
if affected_backup.exists():
    shutil.rmtree(affected_backup)
shutil.copytree(affected_dir, affected_backup)

product = ['lizard.py'] + [
    str(p.relative_to(ROOT / 'target'))
    for area in ('lizard_core', 'lizard_languages', 'lizard_ext')
    for p in sorted((ROOT / 'target' / area).rglob('*.py'))]
hashes = {}
for label, folder in [
        ('target', ROOT / 'target'),
        ('corrected', ROOT / 'corrected'),
        ('baseline', ROOT / 'baseline')]:
    hashes[label] = {
        name: hashlib.sha256((folder / name).read_bytes()).hexdigest()
        for name in product if (folder / name).is_file()}
for name in (
        'lizard_languages/javascript_templates.py',
        'lizard_languages/typescript.py',
        'lizard_languages/tsx.py'):
    assert hashlib.sha256((REPO / name).read_bytes()).hexdigest() == hashes[
        'corrected'][name]
changed = [name for name in product
           if hashes['target'].get(name) != hashes['corrected'].get(name)]
assert set(changed) == {
    'lizard_languages/javascript_templates.py',
    'lizard_languages/typescript.py',
    'lizard_languages/tsx.py'}, changed
(EVIDENCE / 'snapshot-hashes.json').write_text(json.dumps(
    [{'path': name,
      'target': hashes['target'][name],
      'corrected': hashes['corrected'][name],
      'baseline': hashes['baseline'].get(name)}
     for name in changed], indent=2) + '\n')

decision = json.loads((EVIDENCE / "measurement-decision.json").read_text())

results = {}
for phase, baseline in [
        ('incremental', ROOT / 'target'),
        ('cumulative', ROOT / 'baseline')]:
    for workload in ('api', 'api-parameters', 'cli'):
        label = phase + '-' + workload
        if workload == 'api-parameters':
            if affected_dir.exists():
                shutil.rmtree(affected_dir)
            shutil.copytree(consumers_dir, affected_dir)
        else:
            if affected_dir.exists():
                shutil.rmtree(affected_dir)
            shutil.copytree(affected_backup, affected_dir)
        headroom = {
            name: subprocess.check_output(command, text=True)
            for name, command in [('uptime', ['uptime']), ('memory', ['vm_stat'])]}
        (EVIDENCE / (label + '-headroom.json')).write_text(
            json.dumps(headroom, indent=2) + '\n')
        print(label, headroom['uptime'].strip(), flush=True)
        out = ROOT / (label + '.json')
        log_path = EVIDENCE / (label + '-timings.log')
        seconds = '4.0' if workload == 'cli' else '2.5'
        command = [
            sys.executable, str(PAIRED),
            '--baseline', str(baseline),
            '--candidate', str(ROOT / 'corrected'),
            '--workload', workload,
            '--pairs', '20', '--seconds', seconds,
            '--out', str(out)]
        env = dict(**__import__('os').environ)
        env['PYTHONDONTWRITEBYTECODE'] = '1'
        with log_path.open('w') as log:
            result = subprocess.run(
                command, cwd='/private/tmp', stdout=log,
                stderr=subprocess.STDOUT, env=env)
        assert result.returncode == 0, label
        data = json.loads(out.read_text())
        shutil.copyfile(out, EVIDENCE / (label + '.json'))
        summary = {key: data[key] for key in (
            'workload', 'loops', 'medians', 'median_slowdown_pct',
            'paired_95pct_ci', 'passes')}
        results[label] = summary
        print(json.dumps(summary), flush=True)

# Restore primary affected corpus.
if affected_dir.exists():
    shutil.rmtree(affected_dir)
shutil.copytree(affected_backup, affected_dir)
(EVIDENCE / 'measurement-summary.json').write_text(
    json.dumps(results, indent=2) + '\n')
assert all(item['passes'] for item in results.values()), results
print(json.dumps({'all_pass': True, 'workloads': list(results)}, indent=2))
