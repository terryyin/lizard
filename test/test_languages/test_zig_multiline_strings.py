from lizard import analyze_file


def test_multiline_string_lines_do_not_contribute_code():
    code = ('fn real() void {\nconst text =\n'
            r'\\ if (x) fake();' '\n'
            r'\\ } fn fake() void { catch try orelse' '\n'
            ';\n_ = text;\n}\nfn after() void {}')
    functions = analyze_file.analyze_source_code('sample.zig', code).function_list
    assert [(f.name, f.cyclomatic_complexity, f.end_line) for f in functions] == [
        ('real', 1, 7), ('after', 1, 8)]
