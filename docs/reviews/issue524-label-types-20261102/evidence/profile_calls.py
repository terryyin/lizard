"""Profile the exact failing frozen API/parameter workload; not a timing gate."""

import argparse
import cProfile
import hashlib
import importlib
import io
import json
from pathlib import Path
import pstats
import sys


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--checkout', type=Path, required=True)
    parser.add_argument('--corpus', type=Path, required=True)
    parser.add_argument('--out', type=Path, required=True)
    parser.add_argument('--extension')
    args = parser.parse_args()
    root = args.checkout.resolve()
    sys.path.insert(0, str(root))
    lizard = importlib.import_module('lizard')
    analyzer = (lizard.FileAnalyzer(lizard.get_extensions([args.extension]))
                if args.extension else lizard.analyze_file)
    sources = [(str(p), p.read_text()) for p in sorted(args.corpus.rglob('*'))
               if p.is_file()]

    def analyze():
        for _ in range(100):
            for name, source in sources:
                for function in analyzer.analyze_source_code(
                        name, source).function_list:
                    function.parameters
                    function.parameter_count

    profile = cProfile.Profile()
    profile.runcall(analyze)
    modules = {}
    for name, module in list(sys.modules.items()):
        if name == 'lizard' or name.startswith(('lizard_core', 'lizard_languages',
                                               'lizard_ext')):
            file = getattr(module, '__file__', None)
            if file:
                path = Path(file).resolve()
                assert root in path.parents, (name, path)
                modules[name] = str(path)
    args.out.mkdir(parents=True, exist_ok=False)
    profile.dump_stats(str(args.out / 'calls.prof'))
    stats = pstats.Stats(profile)
    rows = []
    for (filename, line, name), (primitive, calls, own, cumulative, callers) in (
            stats.stats.items()):
        if str(root) in filename:
            rows.append({'file': str(Path(filename).relative_to(root)),
                         'line': line, 'name': name, 'primitive': primitive,
                         'calls': calls, 'own': own, 'cumulative': cumulative})
    rows.sort(key=lambda row: row['own'], reverse=True)
    record = {'checkout': str(root), 'loops': 100, 'python': sys.version,
              'extension': args.extension,
              'executable': sys.executable, 'modules': modules, 'rows': rows,
              'corpus': {str(p): hashlib.sha256(p.read_bytes()).hexdigest()
                         for p in sorted(args.corpus.rglob('*')) if p.is_file()}}
    (args.out / 'profile.json').write_text(json.dumps(record, indent=2) + '\n')
    output = io.StringIO()
    pstats.Stats(profile, stream=output).sort_stats('tottime').print_stats(35)
    (args.out / 'top.txt').write_text(output.getvalue())
    print(json.dumps({'out': str(args.out), 'top': rows[:18]}), flush=True)


if __name__ == '__main__':
    main()
