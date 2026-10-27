'''Find-jump scan for keyword/=> regex opens punctuation lookbehind misses.'''

from .js_style_regex_context import (
    _EXPRESSION_KEYWORDS,
    _LINE_ENDS,
    _REGEX_LITERAL_RE,
    _apply_marks,
    _is_property_before,
    _skip_ws_and_comments_back,
    _word_before,
)


def mark_keyword_slashes(source):
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
                if c in line_ends:
                    break
                i += 1
            continue
        if ch == '`':
            i = _skip_template(source, i, n, marks, match_literal, keywords)
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
        i = _consider_slash(source, i, marks, match_literal, keywords)
    return _apply_marks(source, marks)


def _consider_slash(source, i, marks, match_literal, keywords):
    matched = match_literal(source, i)
    if matched is None:
        return i + 1
    prev = _skip_ws_and_comments_back(source, i - 1)
    if prev >= 1 and source[prev] == '>' and source[prev - 1] == '=':
        marks.append(i)
        return matched.end()
    word, before_word = _word_before(source, prev)
    if word is not None and word in keywords and not _is_property_before(
            source, before_word):
        marks.append(i)
    return matched.end()


def _skip_template(source, i, n, marks, match_literal, keywords):
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
                        if x in line_ends:
                            break
                        i += 1
                    continue
                if ch == '`':
                    i = _skip_template(
                        source, i, n, marks, match_literal, keywords)
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
                        source, i, marks, match_literal, keywords)
                    continue
                i += 1
            continue
        i += 1
    return i
