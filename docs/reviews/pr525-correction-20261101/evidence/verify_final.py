"""Run the required full local checks inside one serialized Nix environment."""

import argparse
import hashlib
import json
from pathlib import Path
import subprocess
import sys


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--out', type=Path,
                        default=Path('docs/reviews/pr525-correction-20261101/verification'))
    output = parser.parse_args().out
    output.mkdir(exist_ok=False)
    for name, command in (('pytest', [sys.executable, '-m', 'pytest']),
                          ('make', ['make'])):
        result = subprocess.run(command, capture_output=True, text=True)
        report = dict(command=command, exit=result.returncode,
                      stdout=result.stdout, stderr=result.stderr,
                      stdout_sha256=hashlib.sha256(result.stdout.encode()).hexdigest(),
                      stderr_sha256=hashlib.sha256(result.stderr.encode()).hexdigest())
        (output / (name + '.json')).write_text(json.dumps(report, indent=2) + '\n')
        print(name, 'exit', result.returncode, flush=True)
        for line in (result.stdout + result.stderr).splitlines():
            if ' passed' in line or ' failed' in line or 'error' in line.lower():
                print(line, flush=True)
        if result.returncode:
            return result.returncode
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
