'''Full forward regex-context scan when of/await/for/async structure matters.'''

import re

from .js_style_regex_context import (
    _BASIC_WHITESPACE,
    _JS_WHITESPACE,
    _EXPRESSION_KEYWORDS,
    _LINE_ENDS,
    _before_comments,
    _follows_type_arguments,
    _is_js_dividend,
    _opens_control_header,
    _skip_ws_and_comments_back,
    _skip_ws_back,
    _yield_after_operator,
    _REGEX_DIVISOR,
    _REGEX_LITERAL_RE,
    _RUBY_EXPRESSION_KEYWORDS,
    _STRING_LINE_ENDS,
    _apply_marks,
    _word_before,
)
from .ruby_documentation import match_opaque_span, may_have_documentation


def mark_forward(source, js_family, *, jsx=False, operator_language=None):
    """Full forward context when of/await/for/async structure matters."""
    n = len(source)
    i = 0
    can_regex = True
    expect_property = False
    paren_depth = 0
    for_headers = []
    control_headers = []
    operand_close = None
    async_nesting = 0
    pending_async = False
    pending_for = False
    marks = []
    match_literal = _REGEX_LITERAL_RE.match
    type_comments = {} if operator_language == 'typescript' else None
    keywords = _EXPRESSION_KEYWORDS if js_family else _RUBY_EXPRESSION_KEYWORDS
    whitespace = _JS_WHITESPACE if js_family and not jsx else _BASIC_WHITESPACE
    line_ends = _LINE_ENDS

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
        elif (word == 'yield' and operator_language == 'javascript'
              and _yield_after_operator(source, i - len(word))):
            after_operand()
        elif word in keywords:
            after_operand(next_can_regex=True)
        else:
            after_operand()

    def advance():
        nonlocal i, can_regex, expect_property, pending_async, pending_for
        nonlocal paren_depth, async_nesting, operand_close
        ch = source[i]

        if ch in whitespace:
            i += 1
            return
        if js_family and ch == '/' and i + 1 < n and source[i + 1] == '/':
            start = i
            i += 2
            while i < n and source[i] not in line_ends:
                i += 1
            if type_comments is not None:
                type_comments[_skip_ws_back(source, i - 1)] = start
            return
        if js_family and ch == '/' and i + 1 < n and source[i + 1] == '*':
            start = i
            i += 2
            while i + 1 < n and not (source[i] == '*' and source[i + 1] == '/'):
                i += 1
            i = min(i + 2, n)
            if type_comments is not None:
                type_comments[_skip_ws_back(source, i - 1)] = start
            return
        if not js_family and ch == '#':
            i += 1
            while i < n and source[i] not in '\r\n':
                i += 1
            return
        if ch == '/' and can_regex:
            matched = match_literal(source, i)
            if matched:
                marks.append(i)
                i = matched.end()
                after_operand()
                return
        if operator_language and ch == '/' and not can_regex:
            span = operand_regex_span()
            if span is not None:
                start, i = span
                marks.append(start)
                after_operand()
                return
        if ch in '"\'':
            skip_string(ch)
            after_operand()
            return
        if ch == '`' and js_family:
            scan_template()
            pending_async = False
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
        if ch == '(':
            if operator_language:
                control_headers.append(pending_for or _opens_control_header(source, i))
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
            if control_headers and not control_headers.pop():
                operand_close = i
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
        if (ch in '+-*%~^@' or operator_language and ch == '<'
                or operator_language == 'javascript' and ch == '>'):
            i += 1
            after_operand(next_can_regex=True)
            return
        i += 1
        after_operand()

    def operand_regex_span():
        divisor = _REGEX_DIVISOR.match(source, i)
        if type_comments is not None:
            prev, _ = _before_comments(source, _skip_ws_back(source, i - 1), type_comments)
            if prev >= 0 and source[prev] == '>':
                literal = match_literal(source, i)
                if literal or divisor:
                    if _follows_type_arguments(source, prev, type_comments):
                        return divisor.span(1) if divisor else None
                    return literal.span() if literal else None
        elif divisor is not None:
            prev = _skip_ws_and_comments_back(source, i - 1)
        if divisor is not None:
            word, _ = _word_before(source, prev)
            if prev == operand_close or _is_js_dividend(source, prev, word):
                return divisor.span(1)
        return None

    if not js_family and may_have_documentation(source):
        from .ruby import _PERCENT_LITERAL
        percent_literal = re.compile(_PERCENT_LITERAL, re.S)
        # A quote in documentation must not hide a later regex.
        while i < n:
            if source[i] in '=%_':
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
