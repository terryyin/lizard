"""Observe the PR528 report and valid-division boundaries through Lizard's API."""

import argparse
import sys
from pathlib import Path

parser = argparse.ArgumentParser()
parser.add_argument('--checkout', required=True)
args = parser.parse_args()
sys.path.insert(0, str(Path(args.checkout).resolve()))
from lizard import analyze_file

cases = [
    ('reported return regex', 'js',
     'function a(s) { return /#/.test(s) || s === 1; }', [2]),
    ('reported arrow regex', 'js',
     'function b(xs) { return xs.filter(s => /"/.test(s) || s === "a"); }',
     [2, 1]),
    ('member division', 'js',
     'function divide(obj,a,b,c) { return obj.return / (a || b) / c; }', [2]),
    ('spaced member division', 'js',
     'function divide(obj,a,b,c) { return obj . return / (a || b) / c; }', [2]),
    ('commented member division', 'js',
     'function divide(obj,a,b,c) { return obj. /* member */ return / (a || b) / c; }', [2]),
    ('optional member division', 'js',
     'function divide(obj,a,b,c) { return obj?.return / (a || b) / c; }', [2]),
    ('dollar identifier division', 'js',
     'function divide($return,a,b,c) { return $return / (a || b) / c; }', [2]),
    ('contextual of division', 'js',
     'function divide(of,a,b,c) { return of / (a || b) / c; }', [2]),
    ('non-module await identifier division', 'js',
     'function divide(await,a,b,c) { return await / (a || b) / c; }', [2]),
    ('ordinary division control', 'js',
     'function divide(x,a,b,c) { return x / (a || b) / c; }', [2]),
    ('Ruby return regex', 'rb',
     'def a(s)\n return /#/ =~ s ? 1 : 0\nend', [2]),
    ('Ruby member division', 'rb',
     'def divide(obj,a,b,c)\n obj.return / (a || b) / c\nend', [2]),
]
failures = 0
for label, extension, source, expected in cases:
    extensions = ('js', 'ts', 'jsx', 'tsx') if extension == 'js' else ('rb',)
    for extension in extensions:
        functions = analyze_file.analyze_source_code('a.' + extension, source).function_list
        actual = [f.cyclomatic_complexity for f in functions]
        print(label, extension, 'expected', expected, 'actual', actual)
        failures += actual != expected
raise SystemExit(1 if failures else 0)
