"""Find the boundaries of Kotlin raw strings and string templates."""

import re

from .token_pattern import compiled_token_pattern, tokens_in_span


_BACKTICK_NAME = re.compile(r'`[^`\r\n]+`')
_CODE_MARKS = (
    r'\A#![^\r\n]*|//[^\r\n]*|/\*'
    r"|'(?:\\.|[^'\\])'|" + _BACKTICK_NAME.pattern +
    r'|(?:(?<!\$)\${2,})?"(?:"")?'
)
_STRING_START = re.compile(_CODE_MARKS, re.S)
_EXPRESSION_MARK = re.compile(_CODE_MARKS + r'|[{}]', re.S)
_RAW_MARK = re.compile(r'"{3,}|\$+')
_LINE_MARK = re.compile(r'\\.|"|\$+', re.S)
_COMMENT_MARK = re.compile(r'/\*|\*/')


class _InterpolationBrace(str):
    pass


INTERPOLATION_OPEN = _InterpolationBrace('{')
INTERPOLATION_CLOSE = _InterpolationBrace('}')


def string_tokens(source, addition):
    """Keep literal text opaque and tokenize code inside string templates."""
    pattern = compiled_token_pattern(addition)
    code_start = pos = 0
    while True:
        opening = _STRING_START.search(source, pos)
        if opening is None:
            break
        mark = opening.group()
        pos = opening.end()
        if mark != '/*' and not mark.startswith(('"', '$')):
            continue
        yield from tokens_in_span(pattern, source, code_start, opening.start())
        if mark == '/*':
            pos = _block_comment_end(source, pos)
            yield source[opening.start():pos]
        else:
            pos = yield from _string_tokens(source, opening, pattern)
        code_start = pos
    yield from tokens_in_span(pattern, source, code_start, len(source))


def _string_mode(opening):
    pattern = _RAW_MARK if opening.endswith('"""') else _LINE_MARK
    return pattern, opening.count('$') or 1


def _string_tokens(source, opening, token_pattern):
    # A string frame holds a dollar threshold. A code frame holds the
    # brace depth in the interpolation. Both forms can nest.
    stack = [_string_mode(opening.group())]
    start = opening.start()
    pos = opening.end()
    prefix = ''
    while stack:
        marks, count = stack[-1]
        match = marks.search(source, pos)
        if match is None:
            yield prefix + source[start:]
            return len(source)
        mark = match.group()
        pos = match.end()
        if marks is _EXPRESSION_MARK:
            if mark in ('{', '}'):
                count += 1 if mark == '{' else -1
                if count:
                    stack[-1] = marks, count
                else:
                    yield from tokens_in_span(token_pattern, source, start, match.start())
                    yield INTERPOLATION_CLOSE
                    stack.pop()
                    start = pos
                    # The next fragment has no opening quote of its own.
                    # Mark it as text, even if it starts with // or /*.
                    prefix = '"'
            elif mark == '/*' or mark.startswith('`'):
                yield from tokens_in_span(token_pattern, source, start, match.start())
                pos = _block_comment_end(source, pos) if mark == '/*' else pos
                yield source[match.start():pos]
                start = pos
            elif mark.startswith(('"', '$')):
                yield from tokens_in_span(token_pattern, source, start, match.start())
                stack.append(_string_mode(mark))
                start = match.start()
                prefix = ''
        elif mark.startswith('"'):
            yield prefix + source[start:pos]
            stack.pop()
            start = pos
        elif mark.startswith('$') and len(mark) >= count:
            if source.startswith('{', pos):
                yield prefix + source[start:pos]
                stack.append((_EXPRESSION_MARK, 1))
                yield INTERPOLATION_OPEN
                pos += 1
                start = pos
            elif (name := _BACKTICK_NAME.match(source, pos)) is not None:
                pos = name.end()
    return pos


def _block_comment_end(source, pos):
    depth = 1
    for match in _COMMENT_MARK.finditer(source, pos):
        depth += 1 if match.group() == '/*' else -1
        if depth == 0:
            return match.end()
    return len(source)
