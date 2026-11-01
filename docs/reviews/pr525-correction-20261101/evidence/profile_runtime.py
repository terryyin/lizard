"""Bounded profiles of the same affected public analyzer consumers as timing."""

import argparse
import cProfile
import hashlib
import json
import os
from pathlib import Path
import pstats
import sys


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--checkout', required=True, type=Path)
    parser.add_argument('--corpus', required=True, type=Path)
    parser.add_argument('--out', required=True, type=Path)
    parser.add_argument('--repeat', default=40, type=int)
    args = parser.parse_args()
    checkout = args.checkout.resolve()
    sys.path.insert(0, str(checkout))
    import lizard
    analyzer = lizard.analyze_file
    cases = [(str(p), p.read_text()) for p in sorted(args.corpus.rglob('*'))
             if p.is_file()]
    profiler = cProfile.Profile()
    profiler.enable()
    for _ in range(args.repeat):
        for name, source in cases:
            result = analyzer.analyze_source_code(name, source)
            for function in result.function_list:
                function.parameters
                function.parameter_count
    profiler.disable()
    stats = pstats.Stats(profiler)
    rows = [dict(path=path, line=line, function=function,
                 primitive_calls=data[0], calls=data[1],
                 self_seconds=data[2], cumulative_seconds=data[3])
            for (path, line, function), data in stats.stats.items()]
    report = dict(checkout=str(checkout), imported=lizard.__file__,
                  python=sys.version, repeats=args.repeat, files=len(cases),
                  load=os.getloadavg(), total_seconds=stats.total_tt,
                  production={str(p.relative_to(checkout)):
                              hashlib.sha256(p.read_bytes()).hexdigest()
                              for directory in ('lizard_core',
                                                'lizard_languages', 'lizard_ext')
                              for p in sorted((checkout / directory).rglob('*.py'))},
                  top_self=sorted(rows, key=lambda r: -r['self_seconds'])[:40],
                  bindings=[r for r in rows if 'javascript_bindings.py' in r['path']
                            or 'typescript_states.py' in r['path']])
    args.out.write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(dict(files=len(cases), total_seconds=stats.total_tt,
                          top_self=report['top_self'][:8])))


if __name__ == '__main__':
    main()
