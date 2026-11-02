"""Retain terminal full-check proof without overwriting an earlier attempt."""

import argparse
import hashlib
import json
from pathlib import Path
import subprocess
import sys


def production():
    paths = [Path('lizard.py')]
    for directory in ('lizard_core', 'lizard_languages', 'lizard_ext'):
        paths.extend(Path(directory).rglob('*.py'))
    return {str(p): hashlib.sha256(p.read_bytes()).hexdigest()
            for p in sorted(paths)}


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--out', type=Path, required=True)
    args = parser.parse_args()
    args.out.mkdir(parents=True, exist_ok=False)
    for name, command in (('pytest', [sys.executable, '-m', 'pytest']),
                          ('make', ['make'])):
        before = production()
        result = subprocess.run(command, capture_output=True, text=True)
        after = production()
        record = {'command': command, 'exit': result.returncode,
                  'stdout': result.stdout, 'stderr': result.stderr,
                  'production_before': before, 'production_after': after}
        (args.out / (name + '.json')).write_text(json.dumps(record, indent=2) + '\n')
        print(name, 'exit', result.returncode, flush=True)
        for line in result.stdout.splitlines():
            if ' passed' in line or ' failed' in line:
                print(line, flush=True)
        assert before == after, 'production changed during ' + name
        if result.returncode:
            return result.returncode
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
