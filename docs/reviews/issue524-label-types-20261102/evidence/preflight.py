"""Check frozen inputs through the official isolated worker before timing."""

import argparse
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--cache', type=Path, required=True)
    parser.add_argument('--out', type=Path, required=True)
    args = parser.parse_args()
    args.out.mkdir(parents=True, exist_ok=False)
    worker = Path.cwd() / '.agents/skills/lizard-process-pr/scripts/lizard_measurement.py'
    rows = []
    for name, checkout in [('current', args.cache / 'current'),
                            ('cumulative', args.cache / 'cumulative'),
                            ('candidate', Path.cwd())]:
        for family, workload, extension in [('complete', 'api-parameters', None),
                                             ('complete', 'api-parameters', 'NS'),
                                             ('broad', 'cli', None)]:
            spec = {'checkout': str(checkout.resolve()),
                    'corpus': str((args.cache / family).resolve()),
                    'workload': workload, 'extension': extension}
            label = name + '-' + family + ('-ns' if extension else '')
            with tempfile.TemporaryDirectory(prefix='issue524-preflight-') as tmp:
                path = Path(tmp) / 'spec.json'
                path.write_text(json.dumps(spec))
                command = [sys.executable, '-I', str(worker), str(path)]
                result = subprocess.run(command, input='0\n', cwd=tmp,
                                        capture_output=True, text=True)
            row = {'label': label, 'spec': spec, 'command': command,
                   'exit': result.returncode, 'stdout': result.stdout,
                   'stderr': result.stderr}
            rows.append(row)
            (args.out / 'results.json').write_text(json.dumps({
                'load': os.getloadavg(), 'rows': rows}, indent=2) + '\n')
            print(json.dumps({'label': label, 'exit': result.returncode}), flush=True)
            if result.returncode:
                return 1
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
