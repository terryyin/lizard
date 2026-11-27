import csv
import io
import subprocess
import sys
import tempfile
from pathlib import Path

from lizard import analyze_file


def _analyze_plsql(source_code):
    return analyze_file.analyze_source_code("sample.sql", source_code).function_list


def test_nested_default_call_trailing_formal_and_empty_following_procedure():
    code = (
        "CREATE FUNCTION f(a NUMBER DEFAULT GREATEST(ABS(-1), 2), b NUMBER) "
        "RETURN NUMBER IS BEGIN RETURN a; END;\n"
        "CREATE PROCEDURE g() IS BEGIN NULL; END;\n"
    )
    functions = _analyze_plsql(code)
    assert len(functions) == 2

    func_f, proc_g = functions
    assert func_f.name == "f"
    assert func_f.start_line == 1
    assert func_f.end_line == 1
    assert func_f.cyclomatic_complexity == 1
    assert func_f.parameter_count == 2
    assert func_f.parameters == ["a", "b"]
    assert func_f.full_parameters == [
        "a NUMBER DEFAULT GREATEST ( ABS ( - 1 ) , 2 )",
        "b NUMBER",
    ]
    assert func_f.long_name == (
        "f(a NUMBER DEFAULT GREATEST ( ABS ( - 1 ) , 2 ), b NUMBER)"
    )

    assert proc_g.name == "g"
    assert proc_g.start_line == 2
    assert proc_g.end_line == 2
    assert proc_g.cyclomatic_complexity == 1
    assert proc_g.parameter_count == 0
    assert proc_g.parameters == []
    assert proc_g.full_parameters == []
    assert proc_g.long_name == "g()"


def test_assignment_default_call_and_sized_type_formals():
    code = (
        "CREATE PROCEDURE p("
        "a NUMBER := GREATEST(ABS(-1), 2), "
        "b NUMBER(10, 2) DEFAULT 0, "
        "c VARCHAR2(20)"
        ") IS BEGIN NULL; END;\n"
        "CREATE FUNCTION h() RETURN NUMBER IS BEGIN RETURN 1; END;\n"
    )
    functions = _analyze_plsql(code)
    assert len(functions) == 2

    proc_p, func_h = functions
    assert proc_p.name == "p"
    assert proc_p.cyclomatic_complexity == 1
    assert proc_p.parameter_count == 3
    assert proc_p.parameters == ["a", "b", "c"]
    assert proc_p.full_parameters == [
        "a NUMBER := GREATEST ( ABS ( - 1 ) , 2 )",
        "b NUMBER ( 10 , 2 ) DEFAULT 0",
        "c VARCHAR2 ( 20 )",
    ]
    assert proc_p.long_name == (
        "p(a NUMBER := GREATEST ( ABS ( - 1 ) , 2 ), "
        "b NUMBER ( 10 , 2 ) DEFAULT 0, "
        "c VARCHAR2 ( 20 ))"
    )

    assert func_h.name == "h"
    assert func_h.start_line == 2
    assert func_h.end_line == 2
    assert func_h.parameter_count == 0
    assert func_h.parameters == []
    assert func_h.full_parameters == []
    assert func_h.long_name == "h()"


def test_quoted_formal_name_preserves_source_spelling_and_declaration():
    code = (
        'CREATE PROCEDURE f("x name" IN NUMBER, b OUT VARCHAR2) IS\n'
        "BEGIN NULL; END;\n"
    )
    functions = _analyze_plsql(code)
    assert len(functions) == 1

    proc_f = functions[0]
    assert proc_f.name == "f"
    assert proc_f.cyclomatic_complexity == 1
    assert proc_f.parameter_count == 2
    assert proc_f.parameters == ['"x name"', "b"]
    assert proc_f.full_parameters == ['"x name" IN NUMBER', "b OUT VARCHAR2"]
    assert proc_f.long_name == 'f("x name" IN NUMBER, b OUT VARCHAR2)'


def test_nested_procedure_quoted_punctuation_and_enclosing_boundaries():
    code = (
        "CREATE PROCEDURE outer_proc(p_in NUMBER) IS\n"
        '    PROCEDURE inner_proc("a,)" IN NUMBER := GREATEST(1, 2), '
        "tail OUT VARCHAR2) IS\n"
        "    BEGIN NULL; END;\n"
        "BEGIN NULL; END;\n"
        "CREATE PROCEDURE after_proc(z NUMBER) IS BEGIN NULL; END;\n"
    )
    functions = _analyze_plsql(code)
    assert [f.name for f in functions] == ["inner_proc", "outer_proc", "after_proc"]

    inner_proc, outer_proc, after_proc = functions

    assert inner_proc.start_line == 2
    assert inner_proc.end_line == 3
    assert inner_proc.cyclomatic_complexity == 1
    assert inner_proc.parameter_count == 2
    assert inner_proc.parameters == ['"a,)"', "tail"]
    assert inner_proc.full_parameters == [
        '"a,)" IN NUMBER := GREATEST ( 1 , 2 )',
        "tail OUT VARCHAR2",
    ]
    assert inner_proc.long_name == (
        'inner_proc("a,)" IN NUMBER := GREATEST ( 1 , 2 ), tail OUT VARCHAR2)'
    )

    assert outer_proc.start_line == 1
    assert outer_proc.end_line == 4
    assert outer_proc.cyclomatic_complexity == 1
    assert outer_proc.parameter_count == 1
    assert outer_proc.parameters == ["p_in"]
    assert outer_proc.full_parameters == ["p_in NUMBER"]
    assert outer_proc.long_name == "outer_proc(p_in NUMBER)"

    assert after_proc.start_line == 5
    assert after_proc.end_line == 5
    assert after_proc.cyclomatic_complexity == 1
    assert after_proc.parameter_count == 1
    assert after_proc.parameters == ["z"]
    assert after_proc.full_parameters == ["z NUMBER"]
    assert after_proc.long_name == "after_proc(z NUMBER)"


def test_cli_csv_and_duplicated_param_list_with_quoted_formals():
    with tempfile.TemporaryDirectory() as tmpdir:
        tmp = Path(tmpdir)
        (tmp / "one.sql").write_text(
            'CREATE PROCEDURE proc_one("x name" IN NUMBER DEFAULT GREATEST(1, 2), '
            "b OUT VARCHAR2, c NUMBER, d NUMBER, e NUMBER) IS BEGIN NULL; END;\n",
            encoding="utf-8",
        )
        (tmp / "two.sql").write_text(
            'CREATE PROCEDURE proc_two("x name" NUMBER := 10, '
            "b NUMBER, c VARCHAR2(10), d DATE, e NUMBER DEFAULT 0) IS "
            "BEGIN NULL; END;\n",
            encoding="utf-8",
        )
        (tmp / "three.sql").write_text(
            'CREATE PROCEDURE proc_three("y name" IN NUMBER, '
            "b OUT VARCHAR2, c NUMBER, d NUMBER, e NUMBER) IS BEGIN NULL; END;\n",
            encoding="utf-8",
        )

        completed = subprocess.run(
            [
                sys.executable,
                "-m",
                "lizard",
                "--csv",
                "--verbose",
                "-t",
                "2",
                "-E",
                "duplicated_param_list",
                str(tmp),
            ],
            capture_output=True,
            text=True,
            check=True,
        )

    rows = list(csv.reader(io.StringIO(completed.stdout)))
    header = [col.strip() for col in rows[0]]
    by_name = {
        dict(zip(header, row))["function"]: dict(zip(header, row))
        for row in rows[1:]
    }

    assert set(by_name.keys()) == {"proc_one", "proc_two", "proc_three"}
    for fn_name in ("proc_one", "proc_two", "proc_three"):
        assert by_name[fn_name]["PARAM"] == "5"

    assert by_name["proc_one"]["long_name"] == (
        'proc_one("x name" IN NUMBER DEFAULT GREATEST ( 1 , 2 ), '
        "b OUT VARCHAR2, c NUMBER, d NUMBER, e NUMBER)"
    )
    assert by_name["proc_two"]["long_name"] == (
        'proc_two("x name" NUMBER := 10, b NUMBER, c VARCHAR2 ( 10 ), '
        "d DATE, e NUMBER DEFAULT 0)"
    )
    assert by_name["proc_three"]["long_name"] == (
        'proc_three("y name" IN NUMBER, b OUT VARCHAR2, c NUMBER, d NUMBER, e NUMBER)'
    )

    assert [
        (
            int(by_name[name]["dup_param_list"]),
            int(by_name[name]["dup_param_list_f"]),
        )
        for name in ("proc_one", "proc_two", "proc_three")
    ] == [(2, 2), (2, 2), (1, 1)]
