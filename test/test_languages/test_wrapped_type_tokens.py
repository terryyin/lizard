import csv
import subprocess
import sys
from pathlib import Path

import pytest

from lizard import FileAnalyzer, analyze_file, get_extensions
from lizard_core.file_analyzer import line_counter


def _class_with_functions(first, following, newline="\n"):
    return newline.join(("class Example {", "    " + first, "    " + following, "}"))


@pytest.fixture(params=[
    ("Example.java", "void m(Map<String,%s? extends List<String>> a) {}", "void next() {}"),
    ("Example.cs", "void m(Dictionary<string,%sint?> a) {}", "void next() {}"),
    ("Example.kt", "fun m(a: Map<String,%sInt?>) {}", "fun next() {}"),
], ids=["java", "csharp", "kotlin"])
def wrapped_signature(request):
    return request.param


@pytest.fixture(params=[
    ("\n", "\n    ", 3),
    ("\n", "\n \t\n    ", 4),
    ("\r\n", "\r\n \t\r\n    ", 4),
], ids=["wrapped", "blank-line", "blank-line-crlf"])
def wrapping(request):
    return request.param


@pytest.fixture
def signature_reports(wrapped_signature, wrapping):
    filename, declaration, following = wrapped_signature
    newline, gap, _ = wrapping
    flat = _class_with_functions(declaration % " ", following, newline)
    wrapped = _class_with_functions(declaration % gap, following, newline)
    return (
        analyze_file.analyze_source_code(filename, flat),
        analyze_file.analyze_source_code(filename, wrapped),
    )


def test_wrapped_type_keeps_the_flat_long_name(signature_reports):
    flat, wrapped = signature_reports
    assert wrapped.function_list[0].long_name == flat.function_list[0].long_name


def test_wrapped_type_keeps_the_signature_metrics(signature_reports):
    flat, wrapped = signature_reports
    flat_function = flat.function_list[0]
    wrapped_function = wrapped.function_list[0]
    assert wrapped_function.token_count == flat_function.token_count
    assert wrapped_function.parameter_count == flat_function.parameter_count == 1
    assert wrapped_function.cyclomatic_complexity == flat_function.cyclomatic_complexity == 1


def test_wrapped_type_counts_only_code_lines(signature_reports):
    flat, wrapped = signature_reports
    assert flat.function_list[0].nloc == 1
    assert wrapped.function_list[0].nloc == 2
    assert flat.nloc == 4
    assert wrapped.nloc == 5


def test_wrapped_type_keeps_physical_ranges(signature_reports, wrapping):
    flat, wrapped = signature_reports
    _, _, end_line = wrapping
    assert [(f.start_line, f.end_line) for f in flat.function_list] == [(2, 2), (3, 3)]
    assert [(f.start_line, f.end_line) for f in wrapped.function_list] == [
        (2, end_line), (end_line + 1, end_line + 1),
    ]


GROUPED_VALUES = """\
import java.util.Map;

class GroupedValues {
    static int count(Map<
            String,

            ? extends Number> values) {
        return values.size();
    }
}
"""

ITEMS = """\
import java.util.List;
import java.util.function.Consumer;

class Items {
    static <T> void visit(List<T> items, Consumer<
            ? super T
            > consumer) {
        items.forEach(consumer);
    }
}
"""


@pytest.mark.parametrize(
    "filename, source, long_name, parameters, file_nloc, token_count, start, end",
    [
        (
            "GroupedValues.java", GROUPED_VALUES,
            "GroupedValues::count( Map <String, ? extends Number> values)",
            ["values"], 8, 15, 4, 9,
        ),
        (
            "Items.java", ITEMS,
            "Items::visit( List<T> items , Consumer <? super T> consumer)",
            ["items", "consumer"], 9, 21, 5, 9,
        ),
    ],
    ids=["wrap-after-opening-bracket-and-comma", "wrap-inside-both-brackets"],
)
def test_java_wildcard_reports(
    filename, source, long_name, parameters, file_nloc, token_count, start, end,
):
    result = analyze_file.analyze_source_code(filename, source)
    function, = result.function_list
    assert function.long_name == long_name
    assert function.parameters == parameters
    assert function.parameter_count == len(parameters)
    assert function.cyclomatic_complexity == 1
    assert function.token_count == token_count
    assert function.nloc == 5
    assert result.nloc == file_nloc
    assert (function.start_line, function.end_line) == (start, end)


def test_wrapped_type_works_without_the_line_counter():
    processors = [processor for processor in get_extensions([]) if processor is not line_counter]
    result = FileAnalyzer(processors).analyze_source_code("GroupedValues.java", GROUPED_VALUES)
    function, = result.function_list
    assert function.name == "GroupedValues::count"
    assert result.nloc == 0


def test_csv_reports_the_wrapped_type_and_physical_range(tmp_path):
    source = tmp_path / "Example.java"
    source.write_text(_class_with_functions(
        "void m(Map<String,\n \t\n    ? extends List<String>> a) {}", "void next() {}",
    ), encoding="utf-8")
    script = Path(__file__).resolve().parents[2] / "lizard.py"

    result = subprocess.run(
        [sys.executable, str(script), "--csv", str(source)],
        check=True, capture_output=True, text=True,
    )

    rows = csv.reader(result.stdout.splitlines(keepends=True))
    assert [(row[0], *row[8:11]) for row in rows] == [
        ("2", "Example::m( Map <String, ? extends List<String>> a)", "2", "4"),
        ("1", "Example::next()", "5", "5"),
    ]


@pytest.mark.parametrize(
    "filename, declaration, following, end_line, function_nloc, file_nloc, file_tokens",
    [
        (
            "Example.java", 'void literal() {\nString value = """\nfirst\n\nlast\n""";\n}',
            "void next() {}", 8, 7, 10, 21,
        ),
        (
            "Example.cs", 'void literal() {\nstring value = @"first\n\nlast";\n}',
            "void next() {}", 6, 5, 8, 21,
        ),
        (
            "Example.kt", 'fun literal() {\nval value = """first\n\nlast"""\n}',
            "fun next() {}", 6, 5, 8, 20,
        ),
    ],
    ids=["java-text-block", "csharp-verbatim-string", "kotlin-raw-string"],
)
def test_multiline_literal_reports_keep_their_metrics(
    filename, declaration, following, end_line, function_nloc, file_nloc, file_tokens,
):
    source = _class_with_functions(declaration, following)
    result = analyze_file.analyze_source_code(filename, source)
    literal, after_literal = result.function_list
    assert literal.token_count == 10
    assert literal.nloc == function_nloc
    assert result.nloc == file_nloc
    assert result.token_count == file_tokens
    assert (literal.start_line, literal.end_line) == (2, end_line)
    assert (after_literal.start_line, after_literal.end_line) == (end_line + 1, end_line + 1)
