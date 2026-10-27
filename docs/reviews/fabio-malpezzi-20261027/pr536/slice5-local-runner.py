import datetime
import json
import pathlib
import subprocess
import sys
import time

out = pathlib.Path('/private/tmp/seed016-slice5-qualification')
out.mkdir(exist_ok=True)
expected = '1b8b56e77dce1cb2642db9f3cc957ee6af1d8c94'

def capture(argv):
    return subprocess.check_output(argv, text=True).strip()

def identity():
    return {'head': capture(['git', 'rev-parse', 'HEAD']),
            'tree': capture(['git', 'rev-parse', 'HEAD^{tree}']),
            'status': capture(['git', 'status', '--porcelain'])}

def headroom():
    observations = {}
    for name, argv in [('host', ['hostname']), ('load', ['uptime']),
                       ('cores', ['sysctl', '-n', 'hw.ncpu']),
                       ('memory', ['vm_stat'])]:
        result = subprocess.run(argv, stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT, text=True)
        observations[name] = result.stdout.strip()
    return observations

proof = {'started_utc': datetime.datetime.now(datetime.timezone.utc).isoformat(),
         'before': identity(), 'checks': []}
assert proof['before']['head'] == expected, proof['before']
assert not proof['before']['status'], proof['before']
proof['headroom_before_pytest'] = headroom()
print(json.dumps({'headroom_before_pytest': proof['headroom_before_pytest']}), flush=True)
for label, argv in [('full-pytest', [sys.executable, '-m', 'pytest']),
                    ('make', ['make'])]:
    if label == 'make':
        proof['headroom_before_make'] = headroom()
        print(json.dumps({'headroom_before_make': proof['headroom_before_make']}), flush=True)
    started = time.monotonic()
    result = subprocess.run(argv, stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT, text=True)
    (out / (label + '.log')).write_text(result.stdout)
    check = {'command': argv, 'exit_code': result.returncode,
             'seconds': time.monotonic() - started,
             'output': result.stdout}
    proof['checks'].append(check)
    print(json.dumps({'check': label, 'exit_code': result.returncode,
                      'seconds': check['seconds'],
                      'tail': result.stdout.splitlines()[-12:]}), flush=True)
    (out / 'local-results.json').write_text(json.dumps(proof, indent=2) + '\n')
    if result.returncode:
        sys.exit(result.returncode)
proof['after'] = identity()
proof['finished_utc'] = datetime.datetime.now(datetime.timezone.utc).isoformat()
(out / 'local-results.json').write_text(json.dumps(proof, indent=2) + '\n')
assert proof['after']['head'] == expected, proof['after']
assert proof['after']['tree'] == proof['before']['tree'], proof['after']
assert not proof['after']['status'], proof['after']
print(json.dumps({'final_identity': proof['after'], 'result': 'pass'}), flush=True)
