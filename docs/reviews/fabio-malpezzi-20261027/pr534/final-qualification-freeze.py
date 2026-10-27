"""Freeze exact revisions and both prospective affected corpora without timing."""

import io
import json
import shutil
import subprocess
import sys
import tarfile

from final_qualification_common import (
    CACHE, CHECKOUT, DRIVERS, GATES, HARNESS, HERE, INITIAL, MANIFEST,
    REVISIONS, assert_inputs, command, corpus_hashes, digest,
    now, observations, source_hashes, write_json)


def supplemental_inputs():
    cases = []
    values = [('ternary', 'x = d ? a : b', 3, []),
              ('object', 'x = {a: d}', 2, []),
              ('call', 'x = call({a: d})', 2, []),
              ('initializer_arrow', 'x = (z => ({a: z}))(d)', 2,
               [['(anonymous)', 2, 2, 1]])]
    for label, value, ccn, initializers in values:
        for extension in ('js', 'ts', 'jsx', 'tsx'):
            for alternate_label, alternate in (('bare', 'y => y || d'),
                                                ('group', '(y) => y || d')):
                source = ('function outer(c, d) {\n'
                          f'  const f = c ? ({value}) : {alternate};\n'
                          '  return 1;\n}\nfunction after() { return 1; }\n')
                expected = initializers + [['(anonymous)', 2, 2, 2],
                                            ['outer', 1, 4, ccn], ['after', 5, 5, 1]]
                cases.append(dict(label='query_' + label, extension=extension,
                                  filename=f'query_{label}_{alternate_label}.{extension}',
                                  source=source, expected=expected,
                                  source_test='test_parenthesized_runtime_value_keeps_its_branch_separator'))
    for extension in ('ts', 'tsx', 'vue'):
        for label, parameters in (('typed', '(x: number)'), ('empty', '()')):
            source = ('function outer(c, d) {\n'
                      f'  const f = c ? {parameters} /* parameters */\n'
                      '    : number => 1 : (y) /* parameters */\n'
                      '    : number => y || d;\n'
                      '  return 1;\n}\nfunction after() { return 1; }\n')
            offset = int(extension == 'vue')
            if offset:
                source = '<script lang="ts">\n' + source + '</script>\n'
            expected = [['(anonymous)', 3 + offset, 3 + offset, 1],
                        ['(anonymous)', 4 + offset, 4 + offset, 2],
                        ['outer', 1 + offset, 6 + offset, 2],
                        ['after', 7 + offset, 7 + offset, 1]]
            cases.append(dict(label='trivia_' + label, extension=extension,
                              filename=f'query_trivia_{label}.{extension}',
                              source=source, expected=expected,
                              source_test='test_return_annotation_after_group_trivia'))
    assert len(cases) == 38
    return cases


def main():
    assert command('git', 'rev-parse', 'HEAD') == REVISIONS['candidate']
    assert command('git', 'rev-parse', 'origin/master') == REVISIONS['current']
    assert not command('git', 'diff', '--name-only', '--', 'lizard.py',
                       'lizard_core', 'lizard_languages', 'lizard_ext', 'test')
    initial = json.loads((INITIAL / 'freeze.json').read_text())
    assert digest(INITIAL / 'paired.py') == HARNESS
    assert digest(INITIAL / 'manifest.json') == MANIFEST
    assert not CACHE.exists(), 'A freeze is never overwritten'
    CACHE.mkdir()
    initial_hashes = corpus_hashes(INITIAL / 'corpus')
    assert len(initial_hashes) == 228
    assert len([name for name in initial_hashes if name.startswith('affected/')]) == 128
    for label in ('affected', 'broad', 'addition'):
        assert digest(INITIAL / (label + '-hashes.json')) == initial[label + '_manifest_sha256']
    original = json.loads((INITIAL / 'inputs.json').read_text())
    supplement = supplemental_inputs()
    assert len(original) == 128
    for name, cases in (('original', original), ('supplemental', supplement),
                        ('augmented', original + supplement)):
        write_json(CACHE / (name + '-inputs.json'), cases)
    shutil.copyfile(INITIAL / 'manifest.json', CACHE / 'manifest.json')
    shutil.copyfile(INITIAL.parent / 'seed015-context/base-cli.txt', CACHE / 'cli-reference.txt')
    for name in ('original', 'augmented'):
        runner = CACHE / name
        runner.mkdir()
        shutil.copytree(INITIAL / 'corpus', runner / 'corpus')
        if name == 'augmented':
            for case in supplement:
                path = runner / 'corpus/affected' / case['filename']
                assert not path.exists()
                path.write_text(case['source'])
        for filename in ('paired.py', 'manifest.json'):
            shutil.copyfile(INITIAL / filename, runner / filename)
        hashes = corpus_hashes(runner / 'corpus')
        assert len(hashes) == (228 if name == 'original' else 266)
        assert all(hashes[key] == value for key, value in initial_hashes.items())
        write_json(CACHE / (name + '-corpus-hashes.json'), hashes)
    for label, revision in REVISIONS.items():
        snapshot = CACHE / label
        snapshot.mkdir()
        archive = subprocess.check_output(['git', 'archive', revision], cwd=CHECKOUT)
        with tarfile.open(fileobj=io.BytesIO(archive)) as tree:
            tree.extractall(snapshot)
        write_json(CACHE / (label + '-source-hashes.json'), source_hashes(snapshot))
    shutil.copyfile(HERE / 'qualification-verify.py', CACHE / 'qualification-verify.py')
    freeze = dict(time=now(), revisions=REVISIONS, gates=GATES,
                  harness_sha256=HARNESS, original_manifest_sha256=MANIFEST,
                  drivers={name: digest(HERE / name) for name in DRIVERS},
                  original_inputs=128, supplement_inputs=38, augmented_inputs=166,
                  broad_files=100, python=sys.version, executable=sys.executable,
                  original_inputs_sha256=digest(CACHE / 'original-inputs.json'),
                  supplement_sha256=digest(CACHE / 'supplemental-inputs.json'),
                  augmented_inputs_sha256=digest(CACHE / 'augmented-inputs.json'),
                  cli_reference_sha256=digest(CACHE / 'cli-reference.txt'),
                  cli_expected_change='Only the absolute frozen corpus prefix changes; all three raw outputs equal')
    write_json(CACHE / 'freeze.json', freeze)
    assert_inputs()
    receipt = observations('before')
    assert_inputs()
    write_json(CACHE / 'freeze-observations.json', receipt)
    write_json(CACHE / 'freeze-driver-receipt.json', dict(finished=now(), returncode=0))
    print(json.dumps(dict(frozen=REVISIONS, original=128, supplemental=38,
                          augmented=166, public_candidate_expected=True,
                          original_seven_fields_match=True, returncode=0)), flush=True)


if __name__ == '__main__':
    main()
