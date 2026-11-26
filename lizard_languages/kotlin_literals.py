"""Find the boundaries of Kotlin strings and block comments."""

import re
from itertools import chain

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
_TYPE_TRIVIA = re.compile(r'\s*(/\*|//[^\r\n]*|:)')


class TemplateTokens(chain):  # pylint: disable=R0903
    """A stream that can contain both parts of a Kotlin template."""


class TemplateText(str):
    """A literal fragment for metrics, with the full value on the final fragment."""

    def __new__(cls, text, literal=None):
        token = super().__new__(cls, text)
        token.literal = literal
        return token


class _InterpolationBrace(str):
    pass


INTERPOLATION_OPEN = _InterpolationBrace('{')
INTERPOLATION_CLOSE = _InterpolationBrace('}')


class _NullableQuestion(str):
    is_optional = True


_NULLABLE_QUESTION = _NullableQuestion('?')


def may_have_nested_comment(source):
    """Look for consecutive comment openers with no closer between them."""
    start = source.find('/*')
    while start >= 0:
        next_open = source.find('/*', start + 2)
        if next_open < 0:
            break
        if source.find('*/', start + 2, next_open + 1) < 0:
            return True
        start = next_open
    return False


def literal_tokens(source, addition):
    """Keep literal text opaque and tokenize code inside string templates."""
    pattern = compiled_token_pattern(addition)
    code_start = pos = 0
    while True:
        opening = _STRING_START.search(source, pos)
        if opening is None:
            break
        mark = opening.group()
        pos = opening.end()
        if mark[0] == '`' and _type_follows_block_comment(source, pos):
            yield from tokens_in_span(pattern, source, code_start, opening.start())
            yield mark
            code_start = pos
            continue
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


def _type_follows_block_comment(source, pos):
    # A block comment can end the code span before the type colon.
    has_block_comment = False
    while match := _TYPE_TRIVIA.match(source, pos):
        token = match.group(1)
        if token == ':':
            return has_block_comment
        pos = match.end()
        if token == '/*':
            has_block_comment = True
            pos = _block_comment_end(source, pos)
    return False


def _string_mode(opening, start):
    pattern = _RAW_MARK if opening.endswith('"""') else _LINE_MARK
    return pattern, opening.count('$') or 1, start


def _string_tokens(source, opening, token_pattern):
    # A string frame holds a dollar threshold. A code frame holds the
    # brace depth in the interpolation. Both forms can nest.
    stack = [_string_mode(opening.group(), opening.start())]
    start = opening.start()
    pos = opening.end()
    prefix = ''
    while stack:
        marks, count, literal_start = stack[-1]
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
                    stack[-1] = marks, count, literal_start
                else:
                    yield from tokens_in_span(token_pattern, source, start, match.start(), _expression_token)
                    yield INTERPOLATION_CLOSE
                    stack.pop()
                    start = pos
                    # The next fragment has no opening quote of its own.
                    # Mark it as text, even if it starts with // or /*.
                    prefix = '"'
            elif mark == '/*' or mark.startswith('`'):
                yield from tokens_in_span(token_pattern, source, start, match.start(), _expression_token)
                pos = _block_comment_end(source, pos) if mark == '/*' else pos
                yield source[match.start():pos]
                start = pos
            elif mark.startswith(('"', '$')):
                yield from tokens_in_span(token_pattern, source, start, match.start(), _expression_token)
                stack.append(_string_mode(mark, match.start()))
                start = match.start()
                prefix = ''
        elif mark.startswith('"'):
            fragment = prefix + source[start:pos]
            yield fragment if start == literal_start else TemplateText(fragment, source[literal_start:pos])
            stack.pop()
            start = pos
        elif mark.startswith('$') and len(mark) >= count:
            if source.startswith('{', pos):
                yield TemplateText(prefix + source[start:pos])
                stack.append((_EXPRESSION_MARK, 1, None))
                yield INTERPOLATION_OPEN
                pos += 1
                start = pos
            elif (name := _BACKTICK_NAME.match(source, pos)) is not None:
                pos = name.end()
    return pos


def _expression_token(match):
    # A standalone question mark qualifies a type in Kotlin.
    token = match.group()
    return _NULLABLE_QUESTION if token == '?' else token


def _block_comment_end(source, pos):
    depth = 1
    for match in _COMMENT_MARK.finditer(source, pos):
        depth += 1 if match.group() == '/*' else -1
        if depth == 0:
            return match.end()
    return len(source)
