"""Own semantic subprocesses serially in one Nix environment."""
import json
import os
from pathlib import Path
import subprocess
import sys

ROOT = Path('/Users/terryyin/.cache/lizard-pr-reviews-20261027/pr536-corrected')
CHECKOUT = Path('/Users/terryyin/git/lizard/.worktrees/report-consistent-javascript-and-typescript-arro')
CANDIDATE = 'candidate-5'

if len(sys.argv) > 1:
    snapshot = ROOT / sys.argv[1]
    os.chdir('/private/tmp')
    sys.path.insert(0, str(snapshot))
    from lizard import analyze_file
    from lizard_ext.lizardns import LizardExtension
    names = ['lizard', 'lizard_core.function_info', 'lizard_core.file_analyzer',
             'lizard_languages.javascript_bindings',
             'lizard_languages.javascript_functions',
             'lizard_languages.typescript_states', 'lizard_ext.lizardns']

    def guard():
        for name in names:
            module = __import__(name, fromlist=['__file__'])
            assert snapshot in Path(module.__file__).resolve().parents

    guard()
    result = {}
    for path in sorted((ROOT / 'proof/corpus/affected').iterdir()):
        info = analyze_file.analyze_source_code(str(path), path.read_text())
        result[path.name] = [[f.name, f.long_name, f.parameters, f.parameter_count,
                              f.cyclomatic_complexity, f.nloc, f.token_count,
                              f.start_line, f.end_line] for f in info.function_list]
    guard()
    print(json.dumps(result, sort_keys=True))
    sys.exit(0)


def run(args, filename, expected=0, cwd=CHECKOUT):
    result = subprocess.run(args, cwd=cwd, capture_output=True, text=True)
    (ROOT / filename).write_text(result.stdout + result.stderr)
    print(json.dumps({'command': args, 'output': filename,
                      'returncode': result.returncode}), flush=True)
    assert result.returncode == expected
    return result.stdout


run([sys.executable, '-m', 'pytest', '-q'], CANDIDATE + '-full-pytest.log')
reproduction = run([sys.executable, str(CHECKOUT / 'docs/reviews/fabio-malpezzi-20261027/pr536/reproduce.py'),
                   str(ROOT / CANDIDATE), str(ROOT.with_name('pr536') / 'all-inputs.json')],
                  CANDIDATE + '-reproduce.jsonl', expected=1)
assert json.loads(reproduction.splitlines()[-1])['failures'] == ['arrow-member.js', 'arrow-abandoned.js']
tuples = [run([sys.executable, __file__, label], label + '-tuples.json', cwd='/private/tmp')
          for label in ['candidate-0', CANDIDATE]]
assert json.loads(tuples[0]) == json.loads(tuples[1])
assert len(json.loads(tuples[0])) == 101
targets = [str(ROOT / 'proof/corpus' / x)
           for x in ['lizard.py', 'lizard_core', 'lizard_languages', 'lizard_ext']]
for options, label in [([], 'cli'), (['-ENS'], 'cli-ns')]:
    outputs = [run([sys.executable, str(ROOT / candidate / 'lizard.py')] + options + targets,
                   candidate + '-' + label + '-semantics.log', expected=1,
                   cwd=ROOT / candidate) for candidate in ['candidate-0', CANDIDATE]]
    assert outputs[0] == outputs[1]
run(['node', str(CHECKOUT / '.agents/skills/dough-execute-plan/scripts/consumer-trace.mjs'),
     '--checkout', str(CHECKOUT), '--base', '9276aea2aa337c070169ba40f88b8ce207530243',
     '--term', '_append_parameter', '--term', 'parameter_count', '--term', 'full_parameters',
     '--term', 'parameters', '--term', 'read_token', '--term', 'javascript_bindings',
     '--term', 'typescript_states'], CANDIDATE + '-consumers-raw.md')
print('PASS: full suite, frozen65+CLI exact exclusions, all101 public tuples, broad CLI/ENS parity, trace.')
