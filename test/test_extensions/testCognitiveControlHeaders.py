"""Cognitive complexity of ternaries in brace-language control headers."""

import pytest

from .testCognitiveComplexity import cogc


@pytest.mark.parametrize(("filename", "function", "initializer", "condition"), [
    ("example.js", "function run(a, b) { %s }", "let", "a ? b : 0"),
    ("example.ts", "function run(a: number, b: number) { %s }", "let",
     "a ? b : 0"),
    ("example.jsx", "function run(a, b) { %s }", "let", "a ? b : 0"),
    ("example.vue", "<script>function run(a, b) { %s }</script>", "let",
     "a ? b : 0"),
    ("example.c", "void run(int a, int b) { %s }", "int", "a ? b : 0"),
    ("example.java", "class Example { void run(boolean a, boolean b) { %s } }",
     "int", "a ? b : false"),
    ("example.cs", "class Example { void Run(bool a, bool b) { %s } }",
     "int", "a ? (b) : false"),
], ids=["js", "ts", "jsx", "vue", "c", "java", "cs"])
def test_ternaries_in_if_and_for_headers(filename, function, initializer,
                                         condition):
    body = """
        if (%s) {}
        for (%s i = a ? 0 : 1; i < 10; ++i) {}
    """ % (condition, initializer)
    assert cogc(filename, function % body) == [4]


def test_header_ternary_uses_the_nested_ifs_level():
    assert cogc("example.js", """
        function run(a, b) {
            if (a) {
                if (b ? 1 : 0) {}
            }
        }
    """) == [5]


def test_inner_ternary_nests_under_the_header_ternary():
    assert cogc("example.js", """
        function enabled(options, fallback) {
            if (options ? (options.exact ? options.enabled : fallback)
                        : fallback) return true;
        }
    """) == [4]


def test_ternaries_in_different_for_clauses_are_siblings():
    assert cogc("example.js", """
        function retry(urgent) {
            for (let i = urgent ? 0 : 1; i < (urgent ? 10 : 5); i++) {}
        }
    """) == [3]


def test_ternary_in_while_header():
    assert cogc("example.js", """
        function drain(queue, flush) {
            while (flush ? queue.hasPending() : queue.hasReady()) {
                queue.processNext();
            }
        }
    """) == [2]


def test_class_field_semicolon_does_not_end_the_outer_ternary():
    assert cogc("example.js", """
        function chooseQueue(preferred, fresh, fallback) {
            for (let Queue = preferred ? class { static drained = false; }
                                       : fresh ? fresh : fallback;
                 Queue; Queue = null) {
                return Queue;
            }
            return fallback;
        }
    """) == [4]


def test_object_comma_does_not_end_the_outer_ternary():
    assert cogc("example.js", """
        function accepts(client, input, useDefault, defaults, generated) {
            if (client.accepts(
                input ? { name: input.name, id: input.id }
                      : useDefault ? defaults : generated
            )) return true;
        }
    """) == [4]


@pytest.mark.parametrize("template", [
    "`${prefix}/${route}`",
    "`${prefix, route}`",
])
def test_template_interpolation_does_not_end_the_outer_ternary(template):
    assert cogc("example.js", """
        function verify(check, prefix, route, useDefault, fallback) {
            if (check(prefix ? %s : useDefault ? fallback : route)) return true;
        }
    """ % template) == [4]


def test_class_field_ternaries_are_siblings():
    assert cogc("example.js", """
        function register(registry, enabled, read, write, admin) {
            if (registry.add(class {
                static canRead = enabled ? read : false;
                static canWrite = enabled ? write : false;
                static canDelete = enabled ? admin : false;
                static canInvite = enabled ? admin : false;
            })) return true;
        }
    """) == [5]


def test_object_property_ternaries_are_siblings_inside_the_outer_ternary():
    assert cogc("example.js", """
        function accepts(client, input, name, id, fallback) {
            if (client.accepts(input ? {
                name: name ? name : fallback.name,
                id: id ? id : fallback.id
            } : fallback)) return true;
        }
    """) == [6]


@pytest.mark.parametrize("title", ["title", "first, title"])
def test_jsx_attribute_expression_does_not_end_the_outer_ternary(title):
    assert cogc("example.jsx", """
        function render(visible, compact, first, title) {
            if (layoutFits(visible ? <Card title={%s} />
                                   : compact ? <Compact /> : null)) return true;
            return false;
        }
    """ % title) == [4]


def test_jsx_attribute_of_a_nested_arrow_ends_in_the_enclosing_header():
    assert cogc("example.jsx", """
        function render(visible, compact, title) {
            if (layoutFits(
                visible ? <Card title={() => title ? 1 : 0} />
                        : compact ? <Compact /> : null,
                compact ? <Compact /> : null
            )) return true;
            return false;
        }
    """) == [1, 5]


def test_jsx_attribute_of_a_nested_arrow_preserves_scores_outside_headers():
    assert cogc("example.jsx",
                "function render(visible, compact, title) {"
                "  return visible ? <Card title={() => title ? 1 : 0} />"
                "                 : compact ? <Compact /> : null;"
                "}") == [1, 3]


@pytest.mark.parametrize("operand", ["a", "(a)"])
def test_swift_brace_header_ternary_has_no_extra_nesting(operand):
    assert cogc("example.swift", """
        func run(a: Bool) { if %s ? 1 : 0 {} }
    """ % operand) == [2]


def test_swift_header_ternary_ends_at_the_body_brace():
    assert cogc("example.swift", """
        func run(a: Bool, b: Bool) {
            if a ? 1 : 0 {
                if b {}
            }
        }
    """) == [4]


def test_swift_header_ternary_spans_multiple_lines():
    assert cogc("example.swift", """
        func run(a: Bool, b: Bool) {
            if a ? 1
                 : b ? 2 : 0 {}
        }
    """) == [4]


def test_swift_keeps_the_existing_ternary_recognition():
    assert cogc("example.swift", """
        func run(a: Bool) { if a ? true : false {} }
        func pick(a: Bool) -> Bool { return a ? true : false }
    """) == [1, 0]


def test_optional_chaining_in_a_header_stays_uncounted():
    assert cogc("example.js", "function run(a) { if (a?.b) {} }") == [1]


def test_typescript_conditional_type_adds_a_point_in_a_header_and_return():
    assert cogc("example.ts", """
        function check<T>(input: unknown) {
            if (input as (T extends string ? boolean : number)) return true;
            return false;
        }
        function cast<T>(input: unknown) {
            return input as (T extends string ? boolean : number);
        }
    """) == [2, 1]
