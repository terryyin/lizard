'''Find-jump scan for keyword/=> regex opens punctuation lookbehind misses.'''

import re

from .js_style_regex_context import (
    _EXPRESSION_KEYWORDS,
    _LINE_ENDS,
    REGEX_LITERAL,
    _REGEX_LITERAL_RE,
    _STRING_LINE_ENDS,
    _apply_marks,
    _is_property_before,
    _skip_ws_and_comments_back,
    _word_before,
)


_REGEX_DIVISOR = re.compile(r'/\s+(' + REGEX_LITERAL + ')')


def mark_keyword_slashes(source, *, javascript_operators=False):
    """Find-jump scan: C-level seeks between quotes, templates and slashes."""
    marks = []
    match_literal = _REGEX_LITERAL_RE.match
    keywords = _EXPRESSION_KEYWORDS
    line_ends = _LINE_ENDS
    n = len(source)
    i = 0
    find = source.find
    while i < n:
        j_slash = find('/', i)
        j_dq = find('"', i)
        j_sq = find("'", i)
        j_tmpl = find('`', i)
        candidates = [j for j in (j_slash, j_dq, j_sq, j_tmpl) if j >= 0]
        if not candidates:
            break
        i = min(candidates)
        ch = source[i]
        if ch == '"' or ch == "'":
            quote = ch
            i += 1
            while i < n:
                c = source[i]
                if c == '\\':
                    i += 2
                    continue
                if c == quote:
                    i += 1
                    break
                if c in _STRING_LINE_ENDS:
                    break
                i += 1
            continue
        if ch == '`':
            i = _skip_template(source, i, n, marks, match_literal, keywords, javascript_operators)
            continue
        if i + 1 < n and source[i + 1] in '/*':
            if source[i + 1] == '/':
                i += 2
                while i < n and source[i] not in line_ends:
                    i += 1
            else:
                i += 2
                while i + 1 < n and not (source[i] == '*' and source[i + 1] == '/'):
                    i += 1
                i = i + 2 if i + 1 < n else n
            continue
        i = _consider_slash(source, i, marks, match_literal, keywords, javascript_operators)
    return _apply_marks(source, marks)


def _consider_slash(source, i, marks, match_literal, keywords, javascript_operators):
    matched = match_literal(source, i)
    divisor = _REGEX_DIVISOR.match(source, i) if javascript_operators else None
    if matched is None and divisor is None:
        return i + 1
    prev = _skip_ws_and_comments_back(source, i - 1)
    word, before_word = _word_before(source, prev)
    if matched is not None:
        # '>' can close a type assertion in TypeScript.
        if (prev >= 1 and source[prev - 1:prev + 1] == '=>'
                or javascript_operators and prev >= 0 and source[prev] in '<>'):
            marks.append(i)
            return matched.end()
        if word in keywords and not _is_property_before(source, before_word):
            marks.append(i)
            return matched.end()
    if divisor is not None and prev >= 0 and word not in ('await', 'default', 'extends'):
        # The first slash follows an operand, so the second can open a regex.
        # A ')' can also close a control header before a regex statement.
        operand = source[prev]
        if operand.isalnum() or operand in '_$]"`\'':
            marks.append(divisor.start(1))
            return divisor.end()
    return matched.end() if matched is not None else i + 1


def _skip_template(source, i, n, marks, match_literal, keywords, javascript_operators):
    line_ends = _LINE_ENDS
    i += 1
    while i < n:
        c = source[i]
        if c == '\\':
            i += 2
            continue
        if c == '`':
            return i + 1
        if c == '$' and i + 1 < n and source[i + 1] == '{':
            i += 2
            depth = 1
            while i < n and depth:
                ch = source[i]
                if ch == '"' or ch == "'":
                    quote = ch
                    i += 1
                    while i < n:
                        x = source[i]
                        if x == '\\':
                            i += 2
                            continue
                        if x == quote:
                            i += 1
                            break
                        if x in _STRING_LINE_ENDS:
                            break
                        i += 1
                    continue
                if ch == '`':
                    i = _skip_template(
                        source, i, n, marks, match_literal, keywords, javascript_operators)
                    continue
                if ch == '{':
                    depth += 1
                    i += 1
                    continue
                if ch == '}':
                    depth -= 1
                    i += 1
                    continue
                if ch == '/' and i + 1 < n and source[i + 1] in '/*':
                    if source[i + 1] == '/':
                        i += 2
                        while i < n and source[i] not in line_ends:
                            i += 1
                    else:
                        i += 2
                        while i + 1 < n and not (
                                source[i] == '*' and source[i + 1] == '/'):
                            i += 1
                        i = i + 2 if i + 1 < n else n
                    continue
                if ch == '/':
                    i = _consider_slash(
                        source, i, marks, match_literal, keywords, javascript_operators)
                    continue
                i += 1
            continue
        i += 1
    return i
