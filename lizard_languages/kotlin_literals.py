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


def string_tokens(source, addition):
    """Keep strings as one token and use the shared tokenizer for code."""
    pattern = compiled_token_pattern(addition)
    code_start = pos = 0
    while True:
        opening = _STRING_START.search(source, pos)
        if opening is None:
            break
        mark = opening.group()
        pos = opening.end()
        if mark == '/*':
            pos = _block_comment_end(source, pos)
        elif mark.startswith(('"', '$')):
            pos = _string_end(source, opening)
        else:
            continue
        yield from tokens_in_span(pattern, source, code_start, opening.start())
        yield source[opening.start():pos]
        code_start = pos
    yield from tokens_in_span(pattern, source, code_start, len(source))


def _string_mode(opening):
    pattern = _RAW_MARK if opening.endswith('"""') else _LINE_MARK
    return pattern, opening.count('$') or 1


def _string_end(source, opening):
    # Each frame holds the dollar threshold for a string or the brace depth
    # for an interpolation. Strings can nest inside those expressions.
    stack = [_string_mode(opening.group())]
    pos = opening.end()
    while stack:
        pattern, count = stack[-1]
        match = pattern.search(source, pos)
        if match is None:
            return len(source)
        mark = match.group()
        pos = match.end()
        if pattern is _EXPRESSION_MARK:
            if mark in ('{', '}'):
                count += 1 if mark == '{' else -1
                if count:
                    stack[-1] = pattern, count
                else:
                    stack.pop()
            elif mark == '/*':
                pos = _block_comment_end(source, pos)
            elif mark.startswith(('"', '$')):
                stack.append(_string_mode(mark))
        elif mark.startswith('"'):
            stack.pop()
        elif mark.startswith('$') and len(mark) >= count:
            if source.startswith('{', pos):
                stack.append((_EXPRESSION_MARK, 1))
                pos += 1
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
