"""Single-pass public correspondence and dispatch counts; no timing."""
import argparse
from collections import Counter
import hashlib
import importlib
import json
from pathlib import Path
import sys

parser = argparse.ArgumentParser()
parser.add_argument('--checkout', type=Path, required=True)
parser.add_argument('--corpus', type=Path, required=True)
parser.add_argument('--out', type=Path, required=True)
args = parser.parse_args()
root = args.checkout.resolve()
sys.path.insert(0, str(root))
lizard = importlib.import_module('lizard')
sources = [(str(p), p.read_text()) for p in sorted(args.corpus.rglob('*')) if p.is_file()]
assert len(sources) == 40
analyzers = [(None, lizard.analyze_file), ('NS', lizard.FileAnalyzer(lizard.get_extensions(['NS'])))]
counts = Counter()
observations = []
owner_files = {'typescript_states.py', 'javascript_objects.py', 'typescript.py'}
def observe(frame, event, arg):
    filename = Path(frame.f_code.co_filename).name
    if filename not in owner_files:
        return
    if event == 'call':
        counts[filename + ':' + frame.f_code.co_name] += 1
    elif (event == 'c_call' and filename == 'javascript_objects.py'
          and getattr(arg, '__name__', '') == 'endswith'):
        counts[filename + ':str.endswith'] += 1
sys.setprofile(observe)
for extension, analyzer in analyzers:
    for name, source in sources:
        info = analyzer.analyze_source_code(name, source)
        rows = []
        for function in info.function_list:
            row = {key: getattr(function, key) for key in (
                'name', 'long_name', 'start_line', 'end_line', 'cyclomatic_complexity',
                'nloc', 'token_count', 'parameters', 'full_parameters', 'parameter_count')}
            if extension:
                row['max_nested_structures'] = function.max_nested_structures
            rows.append(row)
        observations.append({'file': name, 'extension': extension, 'nloc': info.nloc,
                             'tokens': info.token_count, 'functions': rows})
sys.setprofile(None)
modules = {}
for name, module in list(sys.modules.items()):
    if name == 'lizard' or name.startswith(('lizard_core', 'lizard_languages', 'lizard_ext')):
        filename = getattr(module, '__file__', None)
        if filename:
            path = Path(filename).resolve()
            assert root in path.parents, (name, path)
            modules[name] = {'path': str(path), 'sha256': hashlib.sha256(path.read_bytes()).hexdigest()}
record = {'command': 'PIP_QUIET=3 nix develop -c python -I ' + ' '.join(sys.argv),
          'python': sys.version, 'executable': sys.executable, 'modules': modules,
          'corpus': {name: hashlib.sha256(Path(name).read_bytes()).hexdigest() for name, _ in sources},
          'loops': 1, 'source_count': len(sources), 'analysis_count': len(observations),
          'observations': observations, 'dispatch_counts': dict(sorted(counts.items())),
          'no_timing': True, 'exit_code': 0}
args.out.write_text(json.dumps(record, indent=2) + '\n')
print(json.dumps({'out': str(args.out), 'source_count':len(sources),
                  'analysis_count':len(observations), 'dispatch_counts':record['dispatch_counts']}))
