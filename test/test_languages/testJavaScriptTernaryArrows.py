"""Public function boundaries for ternary branch arrows and internal colons."""

import pytest

from lizard import analyze_file


def summaries(filename, source):
    return [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
            for f in analyze_file.analyze_source_code(
                filename, source).function_list]


BRANCHES = [
    ('nested_ternary', 'c ? d ? x => x : y => y && d : z => z || c',
     [('f', 1), ('(anonymous)', 2), ('(anonymous)', 2)], 3),
    ('curried_branch', 'c ? x => y => y && x : z => z || c',
     [('f', 2), ('(anonymous)', 2)], 2),
    ('curried_internal_colon',
     'c ? x => d ? y => y : z => z || x : w => w',
     [('f', 3), ('(anonymous)', 1)], 2),
    ('grouped_nested_arrow', 'c ? x => (y => y && x) : z => z || c',
     [('(anonymous)', 2), ('f', 1), ('(anonymous)', 2)], 2),
    ('parenthesized_object', 'c ? x => ({a: x || d}) : y => y && d',
     [('f', 2), ('(anonymous)', 2)], 2),
    ('internal_ternary', 'x => c ? x : d', [('f', 2)], 1),
    ('curried_control', 'x => d ? y => y : z => z || x', [('f', 3)], 1),
    ('block_branches', 'c ? x => { return x; } : y => { return y || d; }',
     [('f', 1), ('(anonymous)', 2)], 2),
    ('plain_ternary', 'c ? d : c || d', [], 3),
]


@pytest.mark.parametrize('extension', ['js', 'ts', 'jsx', 'tsx', 'cjs', 'mjs', 'vue'])
@pytest.mark.parametrize('_label,expression,arrows,outer_ccn', BRANCHES,
                         ids=[case[0] for case in BRANCHES])
def test_branch_colons_and_internal_colons(extension, _label, expression,
                                          arrows, outer_ccn):
    code = (
        'function outer(c, d) {\n'
        f'  const f = {expression};\n'
        '  return 1;\n'
        '}\n'
        'function after() { return 1; }\n'
    )
    offset = 0
    if extension == 'vue':
        code = '<script lang="ts">\n' + code + '</script>\n'
        offset = 1
    expected = [(name, 2 + offset, 2 + offset, ccn) for name, ccn in arrows]
    expected += [('outer', 1 + offset, 4 + offset, outer_ccn),
                 ('after', 5 + offset, 5 + offset, 1)]
    assert summaries('a.' + extension, code) == expected


@pytest.mark.parametrize('extension', ['js', 'ts', 'jsx', 'tsx', 'vue'])
def test_reported_two_parenthesized_branches(extension):
    code = (
        'function choose(strict, b) {\n'
        '  const eq = strict ? (x, y) => x === y : (x, y) => x == y || b;\n'
        '  return eq;\n'
        '}\n'
    )
    offset = 0
    if extension == 'vue':
        code = '<script lang="ts">\n' + code + '</script>\n'
        offset = 1
    assert summaries('a.' + extension, code) == [
        ('(anonymous)', 2 + offset, 2 + offset, 1),
        ('(anonymous)', 2 + offset, 2 + offset, 2),
        ('choose', 1 + offset, 4 + offset, 2),
    ]


@pytest.mark.parametrize('extension', ['jsx', 'tsx'])
def test_jsx_attribute_contains_separate_branch_arrows(extension):
    code = (
        'function view(c, d) {\n'
        ' return <Panel f={c ? x => x : y => y || d}/>;\n'
        '}\n'
    )
    assert summaries('a.' + extension, code) == [
        ('(anonymous)', 2, 2, 1), ('(anonymous)', 2, 2, 2), ('view', 1, 3, 2),
    ]


@pytest.mark.parametrize('extension', ['js', 'ts', 'jsx', 'tsx'])
def test_property_colon_and_computed_key_keep_their_ownership(extension):
    code = (
        'function outer(c, d) {\n'
        '  const o = {f: c ? x => x : y => y || d, next: z => z && c};\n'
        '  return 1;\n'
        '}\n'
        'function after() { return 1; }\n'
    )
    functions = summaries('a.' + extension, code)
    assert functions[0] == ('f', 2, 2, 1)
    assert functions[-2:] == [('outer', 1, 4, 2), ('after', 5, 5, 1)]

    computed = code.replace(
        'f: c ? x => x : y => y || d', '[c ? "a" : "b"]: x => x || d')
    assert summaries('a.' + extension, computed) == [
        ('c?a:b', 2, 2, 2), ('next', 2, 2, 2),
        ('outer', 1, 4, 2), ('after', 5, 5, 1),
    ]
