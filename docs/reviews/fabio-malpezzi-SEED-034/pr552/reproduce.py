"""Public analyze_file observations for target vs corrected PR552 snapshots."""
import json
from pathlib import Path
import sys

ROOT = Path('/Users/terryyin/.cache/lizard-pr-reviews-SEED-034/pr552')
EVIDENCE = Path(__file__).resolve().parent
CASES = json.loads((EVIDENCE / 'inputs.json').read_text())


def row_for(case, functions):
    observed = [
        [f.name, f.start_line, f.end_line, list(f.parameters)]
        for f in functions]
    return {
        'filename': case['filename'],
        'observed': observed,
        'expected': case['expected'],
        'matches': observed == case['expected'],
        'control': bool(case.get('control')),
    }


def observe(label):
    checkout = ROOT / label
    sys.path.insert(0, str(checkout))
    import lizard
    import lizard_languages.javascript_functions as javascript_functions
    import lizard_languages.typescript_declarations as typescript_declarations
    from lizard import analyze_file
    paths = {
        'lizard': lizard.__file__,
        'javascript_functions': javascript_functions.__file__,
        'typescript_declarations': typescript_declarations.__file__,
    }
    root = str(checkout.resolve()) + '/'
    for key, path in paths.items():
        assert str(Path(path).resolve()).startswith(root), (key, path)
    rows = []
    for case in CASES:
        functions = analyze_file.analyze_source_code(
            case['filename'], case['source']).function_list
        rows.append(row_for(case, functions))
    for key, path in paths.items():
        assert str(Path(path).resolve()).startswith(root), (key, path)
    sys.path.pop(0)
    for name in list(sys.modules):
        if name == 'lizard' or name.startswith('lizard.') or name.startswith(
                'lizard_') or name.startswith('lizard_languages') or name.startswith(
                'lizard_core') or name.startswith('lizard_ext'):
            del sys.modules[name]
    return {'label': label, 'paths': paths, 'rows': rows}


def main():
    report = {label: observe(label) for label in ('target', 'corrected', 'raw')}
    (EVIDENCE / 'reproduction.json').write_text(json.dumps(report, indent=2) + '\n')
    summary = {
        label: {
            'all_match': all(row['matches'] for row in data['rows']),
            'claim_match': all(
                row['matches'] for row in data['rows'] if not row['control']),
            'control_match': all(
                row['matches'] for row in data['rows'] if row['control']),
            'mismatches': [
                row['filename'] for row in data['rows'] if not row['matches']],
            'mismatch_detail': [
                {'filename': row['filename'],
                 'observed': row['observed'],
                 'expected': row['expected']}
                for row in data['rows'] if not row['matches']],
        }
        for label, data in report.items()}
    (EVIDENCE / 'reproduction-summary.log').write_text(
        json.dumps(summary, indent=2) + '\n')
    print(json.dumps(summary, indent=2))


if __name__ == '__main__':
    main()
