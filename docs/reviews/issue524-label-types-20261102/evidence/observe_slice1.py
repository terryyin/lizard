"""Record inspected slice-one commitments after the final gates pass."""

import hashlib
import json
from pathlib import Path
import subprocess


ROOT = Path.cwd()
BASE = Path('docs/reviews/issue524-label-types-20261102')
PLAN = '.planning/slice-plans/016-issue524-label-types/PLAN.md'
SCRIPT = '.agents/skills/dough-execute-plan/scripts/story-obligations.mjs'


def main():
    evidence = BASE / 'evidence'
    candidate = json.loads((evidence / 'candidate3.json').read_text())
    for path, digest in candidate['runtime'].items():
        assert hashlib.sha256(Path(path).read_bytes()).hexdigest() == digest, path
    matrix = json.loads((evidence / 'performance5/summary.json').read_text())
    assert len(matrix) == 18 and all(r['passes'] and r['paired_95pct_ci'][1] < 3
                                    for r in matrix)
    for command in ('pytest', 'make'):
        result = json.loads((evidence / ('local5/' + command + '.json')).read_text())
        assert result['exit'] == 0 and result['production_after'] == candidate['runtime']
    assert '## REFACTOR COMPLETE' in (BASE / 'refactor-assertion-role/README.md').read_text()
    status = subprocess.check_output(['git', 'status', '--porcelain'], text=True)
    (evidence / 'slice1-ownership.json').write_text(json.dumps({
        'workspace': str(ROOT), 'branch': subprocess.check_output(
            ['git', 'branch', '--show-current'], text=True).strip(),
        'status': status, 'runtime': candidate['runtime'],
        'boundary': 'Owned isolated execution checkout; selected product/tests '
                    'and planning/qualification records only. Default checkout '
                    'has not been edited; raw evidence remains owned for wrap-up.'},
        indent=2) + '\n')
    observations = {}

    def record(ids, location):
        for identity in ids.split():
            assert identity not in observations
            observations[identity] = location

    tests = 'test/test_languages/testTypeScriptAnnotationBoundaries.py'
    roles = 'test/test_languages/testTypeScriptAssertionRoles.py'
    proof = str(BASE / 'implementation-assertion-role')
    record('C01 C02 C11 C12 C13 C14', tests + ':15-32; actual labels, later decisions, callback/successor tuples, NLOC/tokens; local5 current full checks')
    record('C03 C04 C08 C09 C10', tests + ':35-99 and ' + roles + ':42-77; type-only versus actual field functions, assertion containers, runtime callbacks and exact metrics; retained initial/red/green observations')
    record('C05', 'lizard_languages/typescript_declarations.py:20-31,129-138; runtime/class/declaration/operand/newline roles; ' + roles + ':8-51 actual contextual/genuine assertion controls including export-default')
    record('C06 C07 C15 C16 C17', 'lizard_languages/typescript_type_annotation.py:35-97 and declarations callback/replay; ' + tests + ':60-74 and ' + roles + ':54-77 observe runtime array/comma/brace/operator handoff exactly once')
    record('C18 C19 C20 C21 C22 C23 C24', tests + ':77-99,177-205 and ' + roles + ':42-77; nested/function/generic/template/conditional types, defaults/formals, runtime branches, import/query types and anonymous return arg; current490 actual capture')
    record('C25 C31 C32', tests + ':102-115,226-241; actual NS nesting/nonstrict CCN/optional complex-tags; producer extension correspondence and unchanged frozen40 default/NS80 public records')
    record('C26 C33 C34', tests + ':118-130; real per-suffix CLI CSV/output files and current outputs-after-export.json64 passes')
    record('C27 C28 C29 C30', 'test/test_languages/testJavaScriptBindingBoundaries.py:14-50 authored eleven/original six source journeys, all seven suffixes; current languages-after-export.json1387 passes and local5 full checks')
    record('C35', tests + ':133-144; real Flask /analyze JSON function lists across TS/TSX/Vue; current consumers-after-export.json989 passes')
    record('C36', proof + '/red.json, operator-red.json, left-shift-red.json, export-red.json and cases-after-export.json490; exact public names/formals/spans/CCN distinguish all known losses; prior200/476 unchanged')
    record('C37', str(BASE / 'refactor-assertion-role/README.md') + ' explicit REFACTOR COMPLETE;108 runtime/157 tests/11 files audit,719 retained consumer dispositions and6 current source edges; no further edit')
    record('C38 C39', str(evidence / 'performance5/summary.json') + ' all current/cumulative label/annotation/call/complete API-parameters gates strictly below3; actual public parameter consumption, source/import/stability hashes')
    record('C40', str(evidence / 'performance5/summary.json') + ' all NS families; calls pooled120 retains original40 plus additional80, both additional and pooled strict bounds pass with matching cross-block observations')
    record('C41', str(evidence / 'performance5/summary.json') + ' current/cumulative100-source actual broad CLI gates,4-second calibration and stable path-bearing output')
    record('C42', str(evidence / 'local5/pytest.json') + ' exit0,2917 passed7 existing skips; exact current108 production hashes stable')
    record('C43', str(evidence / 'local5/make.json') + ' exit0,2917 passed7 skips, pycodestyle passes; exit-zero pylint diagnostics retained rather than claimed absent')
    record('C49', str(BASE) + ' raw initial/semantic/operator/export/cost/precision failures retained; representation-correspondence.json preserves exact decoded patch/snapshot bytes; raw archive owned through wrap-up')
    record('C50 Q01', str(evidence / 'qualification-inputs-candidate3.json') + ' frozen638 complete/35 labels/432 annotations/40 calls/100 broad,490 actual contexts; performance5 complete matrix and prospective precision decision, every accepted upper strictly<3')
    record('C51', 'docs/adrs/0001-ubiquitous-language-accepted.md; current490 exact function/CCN/NLOC/token/parameter records, original200 and frozen40/80 unchanged, no public schema/meaning change')
    record('C52', str(evidence / 'slice1-ownership.json') + '; retained isolated checkout identity, declared ownership and selected delivery paths; unrelated/default checkout work preserved')
    record('Q02 Q04', str(BASE / 'implementation') + ' original red/baseline/import receipts before edits and current field/assertion controls; confirmed failures share annotation/member ownership, old green controls preserved')
    record('Q03', 'Plan decisive observations/PFE and current declarations/member/type/signature/CodeStateMachine owners; fresh independent aggregate/cost/assertion reviews confirm existing ownership, no duplicate grammar framework')
    record('Q07 Q08', '.planning/seeds/SEED-024-issue524-label-types.md Goal/Scope/Source and original explicit human instruction authorize implementation/publication; execution identity and remote/trunk in plan')
    record('T01 T02 T03', tests + ':26-32 exact minimal label reports anonymous line1/CCN1/formals[], matching source-stated1.24.1 reference; retained pre-change red drops it; successor separately asserted')
    record('T04 T05 T06 T07', tests + ' and ' + roles + '; actual after/tail span/CCN2 tuples in every journey, branch yes/no and constructor/method successors independently checked')
    record('T08 T09', tests + ':35-57,167-174; type-only fields and genuine initializer preserve constructor/value and s/tail at exact spans/CCN; phantom absence independently checked')
    record('T10 T11', tests + ':60-74 and ' + roles + ':42-77; assertion expression arrows and all runtime successors retain exact names/formals/spans/CCN through runtime handoff')
    record('T12', 'General runtime/declaration/member/operand/type capability owners, not enumerated source switches;490 real variants include contextual/newline/prefix/operator/atomic Vue/nested generic/import/query controls with current source and public proof')
    record('X01 X02 X03', 'Current aggregate diff and independent refactor-assertion-role report: existing runtime bindings/scopes, type-owned bracket stack, streaming tokenizer/replay; no parallel runtime counter, whole-stream buffer or issue-specific switch')
    assert len(observations) == 68
    receipts = []
    for identity, location in observations.items():
        command = ['node', SCRIPT, 'record', '--plan', PLAN,
                   '--commitment', identity, '--observed', location]
        result = subprocess.run(command, capture_output=True, text=True)
        receipts.append({'commitment': identity, 'command': command,
                         'exit': result.returncode, 'stdout': result.stdout,
                         'stderr': result.stderr})
        (evidence / 'slice1-source-observations.json').write_text(
            json.dumps(receipts, indent=2) + '\n')
        assert result.returncode == 0, (identity, result.stderr, result.stdout)
    print(json.dumps({'observed_commitments': len(receipts)}))


if __name__ == '__main__':
    main()
