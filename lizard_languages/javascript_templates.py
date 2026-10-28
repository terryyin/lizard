"""Balanced JavaScript templates, with opaque interpolation expressions."""

import re

from .js_style_regex_expression import REGEX_TOKEN
from .token_pattern import compiled_token_pattern, tokens_in_span


# JavaScript line comments end at any line terminator, even after a backslash.
LINE_COMMENT = r'//[^\r\n\u2028\u2029]*'
# Skip lexical units before interpreting their quotes, backticks or braces.
_OPAQUE_CODE = (
    r'/\*.*?\*/|' + LINE_COMMENT +
    r'|"(?:\\.|[^"\\])*"|\'(?:\\.|[^\'\\])*\''
    r'|' + REGEX_TOKEN
)
_TEMPLATE_START = re.compile(_OPAQUE_CODE + r'|`', re.S)
_EXPRESSION_SPECIAL = re.compile(_OPAQUE_CODE + r'|[`{}]', re.S)
_TEMPLATE_SPECIAL = re.compile(r'\\.|`|\$\{', re.S)
_WHOLE_TOKEN = re.compile(r'.*', re.S)


def _template_end(source, start):
    """Return end and outer expression spans, using a stack instead of recursion."""
    # Zero denotes template text; positive values count interpolation braces.
    stack = [0]
    expressions = []
    expression_start = None
    pos = start + 1
    while stack:
        pattern = _TEMPLATE_SPECIAL if stack[-1] == 0 else _EXPRESSION_SPECIAL
        match = pattern.search(source, pos)
        if match is None:
            return len(source), None
        token = match.group()
        pos = match.end()
        if stack[-1] == 0:
            if token == '`':
                stack.pop()
            elif token == '${':
                if len(stack) == 1:
                    expression_start = pos
                stack.append(1)
        elif token == '`':
            stack.append(0)
        elif token == '{':
            stack[-1] += 1
        elif token == '}':
            stack[-1] -= 1
            if stack[-1] == 0:
                stack.pop()
                if len(stack) == 1:
                    expressions.append((expression_start, match.start()))
    return pos, expressions


def _template_spans(source):
    pos = 0
    while True:
        match = _TEMPLATE_START.search(source, pos)
        if match is None:
            return
        pos = match.end()
        if match.group() == '`':
            end, expressions = _template_end(source, match.start())
            yield match.start(), end, expressions
            pos = end


def _split_template(source, start, end, expressions):
    yield '`'
    pos = start + 1
    for expression_start, expression_end in expressions:
        text_end = expression_start - 2
        if pos < text_end:
            yield '`' + source[pos:text_end] + '`'
        yield '${'
        yield source[expression_start:expression_end]
        yield '}'
        pos = expression_end + 1
    if pos < end - 1:
        yield '`' + source[pos:end - 1] + '`'
    yield '`'


def _converted_template(source, start, end, token_class, split):
    token = token_class(_WHOLE_TOKEN.match(source, start, end))
    if (split and isinstance(token, str) and token.startswith('`')
            and token.endswith('`') and len(token) > 1):
        token_end, expressions = _template_end(token, 0)
        if expressions is not None:
            yield from _split_template(token, 0, token_end, expressions)
            return
    yield token


def interpolation_code(source, in_expression=False):
    """Spans of the code in the interpolations of a template, at any depth.

    The source is a whole template, or the expression of an interpolation.
    Nested templates split the code around them, so that each character is
    scanned once.
    """
    if not (in_expression or (source.startswith('`') and '${' in source)):
        return
    stack, pos, code = ([1], 0, 0) if in_expression else ([0], 1, None)
    while stack:
        pattern = _TEMPLATE_SPECIAL if stack[-1] == 0 else _EXPRESSION_SPECIAL
        match = pattern.search(source, pos)
        if match is None:
            break
        token = match.group()
        pos = match.end()
        if stack[-1] == 0:
            if token == '${':
                stack.append(1)
                code = pos
            elif token == '`':
                stack.pop()
                code = pos
        elif token == '`':
            yield code, match.start()
            stack.append(0)
        elif token == '{':
            stack[-1] += 1
        elif token == '}':
            stack[-1] -= 1
            if stack[-1] == 0:
                yield code, match.start()
                stack.pop()
    if stack and stack[-1]:
        yield code, len(source)


def template_tokens(source, addition='', token_class=None, split=True):
    """Tokenize ordinary spans normally; TSX retains atomic template tokens."""
    pattern = compiled_token_pattern(addition)
    pos = 0
    if '`' in source:
        for start, end, expressions in _template_spans(source):
            yield from tokens_in_span(pattern, source, pos, start, token_class)
            if split and expressions is not None and token_class is None:
                yield from _split_template(source, start, end, expressions)
            elif token_class is None:
                yield source[start:end]
            else:
                yield from _converted_template(
                    source, start, end, token_class, split)
            pos = end
    yield from tokens_in_span(pattern, source, pos, len(source), token_class)
