'''Full forward regex-context scan when of/await/for/async structure matters.'''

import re

from .js_style_regex_context import (
    _BASIC_WHITESPACE,
    _EXPRESSION_KEYWORDS,
    _JS_WHITESPACE,
    _LINE_ENDS,
    _REGEX_LITERAL_RE,
    _RUBY_EXPRESSION_KEYWORDS,
    _STRING_LINE_ENDS,
    _apply_marks,
    _word_before,
)
from .ruby_documentation import match_opaque_span, may_have_documentation


def mark_forward(source, js_family, *, jsx=False):
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
        elif word in keywords:
            after_operand(next_can_regex=True)
        else:
            after_operand()

    def advance():
        nonlocal i, can_regex, expect_property, pending_async, pending_for
        nonlocal paren_depth, async_nesting
        ch = source[i]

        if ch in whitespace:
            i += 1
            return
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
        if ch == '!' and js_family and not can_regex and _after_assertion_operand(source, i):
            i += 1
            after_operand()
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
        if not js_family or ch not in _JS_NON_ASCII_SPACES:
            after_operand()

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


_JS_NON_ASCII_SPACES = frozenset(
    '\u00a0\u1680\u2000\u2001\u2002\u2003\u2004\u2005'
    '\u2006\u2007\u2008\u2009\u200a\u202f\u205f\u3000\ufeff')
_ASSERTION_SPACES = _JS_NON_ASCII_SPACES | frozenset(' \t\v\f')


def _after_assertion_operand(source, pos):
    """A same-line identifier, index or assertion can take a postfix '!'."""
    operand = pos - 1
    while operand >= 0 and source[operand] in _ASSERTION_SPACES:
        operand -= 1
    if operand < 0:
        return False
    character = source[operand]
    if character in ']!':
        return True
    if not (character.isalnum() or character in '_$'):
        return False
    # A module can use prefix '!' after 'export default' or top-level 'await'.
    word, before = _word_before(source, operand)
    return word not in ('await', 'default') or (before >= 0 and source[before] == '.')
