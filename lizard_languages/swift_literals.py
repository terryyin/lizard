"""Swift string literals and nested block comments.

A regular expression cannot delimit either one, because interpolation and
block comments both nest. Line comments stay with the surrounding code.
Interpolated expressions (`\\(...)`) are code inside a string literal.
"""

import re


_LITERAL_START = re.compile(r'//|/\*|#*"')
_COMMENT_DELIMITER = re.compile(r'/\*|\*/')
_STRING_SPECIAL = re.compile(r'[\\"\n]')
_INTERPOLATION_SPECIAL = re.compile(r'//|/\*|[()\n]|#*"')


def literal_spans(source, start=0, length=None):
    """Yield (is_literal, start, end) spans covering source[start:length]."""
    if length is None:
        length = len(source)
    pos = start
    while True:
        match = _LITERAL_START.search(source, pos, length)
        if match is None:
            break
        if match.group() == '//':
            newline = source.find('\n', match.end(), length)
            pos = length if newline < 0 else newline
            continue
        if match.group() == '/*':
            end = _block_comment_end(source, match.start())
        else:
            end = _string_end(source, match.start(), len(match.group()) - 1)
        end = min(end, length)
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


def string_parts(source, start, end):
    """Yield (is_code, start, end) spans covering the string literal
    source[start:end]: literal text, and each interpolated expression
    with its own parentheses, from the `(` after `\\` to its closing `)`."""
    hashes = 0
    while source.startswith('#', start + hashes):
        hashes += 1
    interpolations = []
    _string_end(source, start, hashes, interpolations)
    pos = start
    for code_start, code_end in interpolations:
        yield False, pos, code_start
        yield True, code_start, code_end
        pos = code_end
    if pos < end:
        yield False, pos, end


def _string_end(source, start, hashes, interpolations=None):
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
                code = pos
                pos = _interpolation_end(source, pos, multiline)
                if interpolations is not None:
                    interpolations.append((code, pos))
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
        elif token == '/*':
            # Parentheses in comments do not close the interpolation.
            pos = _block_comment_end(source, match.start())
        elif token == '//':
            newline = source.find('\n', pos)
            pos = len(source) if newline < 0 else newline
        else:
            pos = _string_end(source, match.start(), len(token) - 1)
