import argparse
import hashlib
import json
import math
import os
from pathlib import Path
import statistics
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parent
CORPUS = ROOT / 'corpus'

def worker(checkout, loops, workload):
    sys.path.insert(0, str(Path(checkout).resolve()))
    from lizard import analyze_file, FileAnalyzer, get_extensions
    from lizard_ext.lizardns import LizardExtension
    if workload == 'python-ns':
        files = list((CORPUS / 'affected').glob('*.py'))
        analyzer = FileAnalyzer(get_extensions([LizardExtension()]))
    else:
        files = list((CORPUS / 'affected').glob('*'))
        analyzer = analyze_file
    cases = [(str(p), p.read_text()) for p in sorted(files)]
    def batch(count):
        start = time.perf_counter()
        for _ in range(count):
            for name, source in cases:
                result = analyzer.analyze_source_code(name, source)
                if workload == 'api-parameters':
                    for function in result.function_list:
                        function.parameters
                        function.parameter_count
        return time.perf_counter() - start
    batch(3)
    for line in sys.stdin:
        count = int(line)
        print(json.dumps({'seconds': batch(count), 'files': len(cases)}), flush=True)

def run():
    p = argparse.ArgumentParser()
    p.add_argument('--baseline', required=True)
    p.add_argument('--candidate', required=True)
    p.add_argument('--workload', choices=['python-ns', 'api', 'api-parameters', 'cli-ns', 'cli'], required=True)
    p.add_argument('--pairs', type=int, default=20)
    p.add_argument('--seconds', type=float, default=1.2)
    p.add_argument('--out', required=True)
    args = p.parse_args()
    paths = [str(Path(args.baseline).resolve()), str(Path(args.candidate).resolve())]
    workers = []
    if args.workload in ('api', 'api-parameters', 'python-ns'):
        for path in paths:
            workers.append(subprocess.Popen([sys.executable, __file__, 'worker', path, '0', args.workload], stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True))
        def measure(which, loops):
            workers[which].stdin.write(str(loops) + '\n')
            workers[which].stdin.flush()
            return json.loads(workers[which].stdout.readline())['seconds']
    else:
        targets = [str(CORPUS / x) for x in ('lizard.py', 'lizard_core', 'lizard_languages', 'lizard_ext')]
        options = ['-ENS'] if args.workload == 'cli-ns' else []
        def measure(which, loops):
            command = [sys.executable, str(Path(paths[which]) / 'lizard.py')] + options + targets
            start = time.perf_counter()
            for _ in range(loops):
                result = subprocess.run(command, cwd=paths[which], stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
                if result.returncode not in (0, 1):
                    raise RuntimeError(result.stderr.decode())
            return time.perf_counter() - start
    pilots = [measure(i, 1) for i in range(2)]
    loops = max(1, math.ceil(args.seconds / min(pilots)))
    for i in range(2): measure(i, loops)
    raw = []
    try:
        for pair in range(args.pairs):
            row = {}
            for which in ([0, 1] if pair % 2 == 0 else [1, 0]):
                row[['baseline', 'candidate'][which]] = measure(which, loops)
            raw.append(row)
            print(json.dumps({'pair': pair + 1, **row}), flush=True)
    finally:
        for process in workers:
            process.stdin.close()
            process.wait()
    logs = [math.log(r['candidate'] / r['baseline']) for r in raw]
    mean = statistics.mean(logs)
    se = statistics.stdev(logs) / math.sqrt(len(logs))
    # Two-sided 95% t interval; 2.093 is t(19) and conservative for >=20 pairs.
    interval = [(math.exp(mean + sign * 2.093 * se) - 1) * 100 for sign in (-1, 1)]
    medians = {label: statistics.median(r[label] for r in raw) for label in ('baseline', 'candidate')}
    result = {'command': sys.argv, 'python': sys.version, 'executable': sys.executable,
              'host': os.uname().nodename, 'workload': args.workload, 'paths': paths,
              'pilots': pilots, 'loops': loops, 'raw': raw, 'medians': medians,
              'stdev': {label: statistics.stdev(r[label] for r in raw) for label in medians},
              'median_slowdown_pct': (medians['candidate'] / medians['baseline'] - 1) * 100,
              'paired_geometric_slowdown_pct': (math.exp(mean) - 1) * 100,
              'paired_95pct_ci': interval, 'passes': interval[1] < 3,
              'manifest_sha256': hashlib.sha256((ROOT / 'manifest.json').read_bytes()).hexdigest()}
    Path(args.out).write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps({k: v for k, v in result.items() if k != 'raw'}), flush=True)

if __name__ == '__main__':
    if len(sys.argv) > 1 and sys.argv[1] == 'worker':
        worker(*sys.argv[2:])
    else:
        run()
