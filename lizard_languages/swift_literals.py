"""Swift string literals and nested block comments.

A regular expression cannot delimit either one, because interpolation and
block comments both nest. Line comments stay with the surrounding code.
"""

import re


_LITERAL_START = re.compile(r'//|/\*|#*"')
_COMMENT_DELIMITER = re.compile(r'/\*|\*/')
_STRING_SPECIAL = re.compile(r'[\\"\n]')
_INTERPOLATION_SPECIAL = re.compile(r'[()\n]|#*"')


def literal_spans(source):
    """Yield (is_literal, start, end) spans covering source."""
    start = pos = 0
    length = len(source)
    while True:
        match = _LITERAL_START.search(source, pos)
        if match is None:
            break
        if match.group() == '//':
            newline = source.find('\n', match.end())
            pos = length if newline < 0 else newline
            continue
        if match.group() == '/*':
            end = _block_comment_end(source, match.start())
        else:
            end = _string_end(source, match.start(), len(match.group()) - 1)
        if start < match.start():
            yield False, start, match.start()
        yield True, match.start(), end
        start = pos = end
    if start < length or length == 0:
        yield False, start, length


def _block_comment_end(source, start):
    depth = 0
    for match in _COMMENT_DELIMITER.finditer(source, start):
        depth += 1 if match.group() == '/*' else -1
        if depth == 0:
            return match.end()
    return len(source)


def _string_end(source, start, hashes):
    quote = start + hashes
    multiline = source.startswith('"""', quote)
    close = ('"""' if multiline else '"') + '#' * hashes
    escape = '\\' + '#' * hashes
    pos = quote + (3 if multiline else 1)
    while True:
        match = _STRING_SPECIAL.search(source, pos)
        if match is None:
            return len(source)
        pos = match.start()
        if source.startswith(close, pos):
            return pos + len(close)
        if source.startswith(escape, pos):
            pos += len(escape)
            if source.startswith('(', pos):
                pos = _interpolation_end(source, pos, multiline)
            else:
                pos += 1
        elif source[pos] == '\n' and not multiline:
            return pos
        else:
            pos += 1


def _interpolation_end(source, start, multiline):
    depth = 0
    pos = start
    while True:
        match = _INTERPOLATION_SPECIAL.search(source, pos)
        if match is None:
            return len(source)
        token = match.group()
        pos = match.end()
        if token == '(':
            depth += 1
        elif token == ')':
            depth -= 1
            if depth == 0:
                return pos
        elif token == '\n':
            if not multiline:
                return match.start()
        else:
            pos = _string_end(source, match.start(), len(token) - 1)
