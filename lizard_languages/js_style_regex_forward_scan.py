'''Full forward regex-context scan when of/await/for/async structure matters.'''

import re

from .js_style_regex_context import (
    _EXPRESSION_KEYWORDS,
    _LINE_ENDS,
    _REGEX_LITERAL_RE,
    _RUBY_EXPRESSION_KEYWORDS,
    _STRING_LINE_ENDS,
    _apply_marks,
)
from .ruby_documentation import match_opaque_span, may_have_documentation
from . import ruby_regexp


def mark_forward(source, js_family):
    """Full forward context when of/await/for/async structure matters."""
    n = len(source)
    i = 0
    can_regex = True
    expect_property = False
    paren_depth = 0
    for_headers = []
    async_nesting = 0
    pending_async = False
    pending_for = False
    marks = []
    match_literal = _REGEX_LITERAL_RE.match
    keywords = _EXPRESSION_KEYWORDS if js_family else _RUBY_EXPRESSION_KEYWORDS
    line_ends = _LINE_ENDS
    append_operator = None if js_family else re.compile(r"<<[^\S\r\n]*/")
    ruby_append = append_operator is not None and append_operator.search(source) is not None
    ruby_line_has_code = False
    if not js_family:
        match_literal = ruby_regexp.LITERAL.match

    def after_operand(next_can_regex=False, next_expect_property=False):
        nonlocal can_regex, expect_property, pending_async
        can_regex = next_can_regex
        expect_property = next_expect_property
        pending_async = False

    def skip_string(quote):
        nonlocal i
        i += 1
        while i < n:
            ch = source[i]
            if ch == '\\':
                i += 2
                continue
            if ch == quote:
                i += 1
                return
            if js_family and ch in _STRING_LINE_ENDS:
                return
            i += 1

    def scan_expression(stop_below):
        nonlocal i
        brace_depth = stop_below
        after_operand(next_can_regex=True)
        while i < n:
            ch = source[i]
            if ch == '{':
                brace_depth += 1
                i += 1
                after_operand(next_can_regex=True)
            elif ch == '}':
                brace_depth -= 1
                i += 1
                after_operand()
                if brace_depth < stop_below:
                    return
            else:
                advance()

    def scan_template():
        nonlocal i
        i += 1
        while i < n:
            ch = source[i]
            if ch == '\\':
                i += 2
            elif ch == '`':
                i += 1
                after_operand()
                return
            elif ch == '$' and i + 1 < n and source[i + 1] == '{':
                i += 2
                scan_expression(1)
            else:
                i += 1
        after_operand()

    def read_word():
        nonlocal i
        start = i
        i += 1
        while i < n and (source[i].isalnum() or source[i] in '_$'):
            i += 1
        return source[start:i]

    def after_word(word):
        nonlocal can_regex, pending_async, pending_for, async_nesting
        if expect_property:
            after_operand()
        elif ruby_append and word == 'def':
            after_operand(next_expect_property=True)
        elif word == 'async' and js_family:
            pending_async = True
            can_regex = False
        elif word == 'function':
            if pending_async:
                async_nesting += 1
                pending_async = False
            can_regex = False
        elif word == 'for' and js_family:
            pending_for = True
            after_operand()
        elif (word == 'of' and js_family and for_headers
              and paren_depth == for_headers[-1]):
            for_headers.pop()
            after_operand(next_can_regex=True)
        elif word == 'await' and js_family and async_nesting > 0:
            after_operand(next_can_regex=True)
        elif word in keywords:
            after_operand(next_can_regex=True)
        else:
            after_operand()

    def advance():
        nonlocal ruby_line_has_code
        nonlocal i, can_regex, expect_property, pending_async, pending_for
        nonlocal paren_depth, async_nesting
        ch = source[i]
        if ch == '\n' and not js_family:
            ruby_line_has_code = False

        if ch in ' \t\v\f' or ch in line_ends:
            i += 1
            return
        if ch == '\\' and not js_family:
            if source.startswith('\\\n', i):
                i += 2
                return
            if source.startswith('\\\r\n', i):
                i += 3
                return
        ruby_statement_start = not js_family and not ruby_line_has_code
        ruby_line_has_code = True
        if js_family and ch == '/' and i + 1 < n and source[i + 1] == '/':
            i += 2
            while i < n and source[i] not in line_ends:
                i += 1
            return
        if js_family and ch == '/' and i + 1 < n and source[i + 1] == '*':
            i += 2
            while i + 1 < n and not (source[i] == '*' and source[i + 1] == '/'):
                i += 1
            i = min(i + 2, n)
            return
        if not (js_family or can_regex or ruby_statement_start) and source.startswith('//', i):
            # Division can touch the opening slash of its right operand.
            i += 1
            after_operand(next_can_regex=True)
            return
        if not js_family and ch == '#':
            i += 1
            while i < n and source[i] not in '\r\n':
                i += 1
            return
        if ch == '/' and (can_regex or ruby_statement_start):
            matched = match_literal(source, i)
            if matched and not can_regex and '#{' in matched[0]:
                # An inner slash may end the pattern before the interpolation.
                if '#{' in ruby_regexp.COMPLETE_INTERPOLATION.sub('', matched[0]):
                    matched = None
            if matched:
                marks.append(i)
                i = matched.end()
                after_operand()
                return
        if ch in '"\'':
            skip_string(ch)
            after_operand()
            return
        if ch == ':' and ruby_append and can_regex and source.startswith(':`', i):
            # In an operand position, a colon and backtick name an operator.
            i += 2
            after_operand()
            return
        if ch == '`':
            if js_family:
                scan_template()
                pending_async = False
                return
            if ruby_append and not expect_property:
                skip_string(ch)
                after_operand()
                return
        if source.startswith('=>', i):
            i += 2
            after_operand(next_can_regex=True)
            return
        if source.startswith('?.', i):
            i += 2
            after_operand(next_expect_property=True)
            return
        if source.startswith('++', i) or source.startswith('--', i):
            i += 2
            after_operand()
            return
        if ch.isalpha() or ch in '_$':
            after_word(read_word())
            return
        if ch.isdigit():
            i += 1
            while i < n and (source[i].isalnum() or source[i] in '._'):
                i += 1
            after_operand()
            return
        if ch == '<' and ruby_append and not expect_property:
            append = append_operator.match(source, i)
            matched = match_literal(source, append.end() - 1) if append else None
            # A slash inside executable interpolation can end this match early.
            if matched and '#{' not in matched[0]:
                marks.append(matched.start())
                i = matched.end()
                after_operand()
                return
        if ch == '(':
            if pending_for:
                for_headers.append(paren_depth + 1)
                pending_for = False
            if pending_async:
                async_nesting += 1
                pending_async = False
            paren_depth += 1
            i += 1
            can_regex = True
            expect_property = False
            return
        if ch == ')':
            paren_depth = max(0, paren_depth - 1)
            while for_headers and for_headers[-1] > paren_depth:
                for_headers.pop()
            i += 1
            after_operand()
            return
        if ch == '.':
            i += 1
            after_operand(next_expect_property=True)
            return
        if ch == '@' and not js_family:
            # A Ruby `@when` or `@@if` is a variable, so the word is a name.
            while i < n and source[i] == '@':
                i += 1
            after_operand(next_expect_property=True)
            return
        if ch in '=,({[?:!&|;':
            i += 1
            after_operand(next_can_regex=True)
            return
        if ch == '}':
            if async_nesting and paren_depth == 0:
                async_nesting = max(0, async_nesting - 1)
            i += 1
            after_operand()
            return
        if ch in '];':
            i += 1
            after_operand(next_can_regex=(ch == ';'))
            return
        if ch in '+-*%~^@':
            i += 1
            after_operand(next_can_regex=True)
            return
        i += 1
        after_operand()

    ruby_heredocs = not js_family and '<<' in source
    ruby_documentation = not js_family and (ruby_append or may_have_documentation(source))
    if ruby_heredocs or ruby_documentation:
        from .ruby import _PERCENT_LITERAL, _plain_heredoc_at
        percent_literal = re.compile(_PERCENT_LITERAL, re.S)
        # A quote in documentation must not hide a later regex.
        while i < n:
            if ruby_heredocs and source.startswith('<<', i):
                heredoc = _plain_heredoc_at(source, i)
                if heredoc:
                    after_operand()
                    i = heredoc.end()
                    continue
            if ruby_documentation and source[i] in '=%_':
                span = match_opaque_span(source, i, percent_literal)
                if span:
                    if source[i] == '%':
                        after_operand()
                    i = span.end()
                    continue
            advance()
    else:
        while i < n:
            advance()
    return _apply_marks(source, marks)
