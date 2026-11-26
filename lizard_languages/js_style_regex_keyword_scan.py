'''Find regex openings after keywords, arrows and comments.'''

import re

from .js_style_regex_context import (
    _EXPRESSION_KEYWORDS,
    _EXPRESSION_PUNCTUATION,
    _LINE_TERMINATORS,
    _REGEX_LITERAL_RE,
    _STRING_LINE_ENDS,
    _apply_marks,
    _is_property_before,
    _skip_ws_and_comments_back,
    _skip_ws_back,
    _word_before,
)

_COMMENT = re.compile(rf'//[^{_LINE_TERMINATORS}]*|/\*.*?(?:\*/|\Z)', re.S)
_LINE_BREAK = re.compile(rf'[{_LINE_TERMINATORS}]')
# Match an operator before an operand. Compound assignments and arrows can
# precede yield, as can a single question mark in a conditional expression.
_OPERAND_OPERATOR = re.compile(r'(?:[=!]=|(?<!<)<=|(?<!>)>=|(?<!=)>|\?\?|[<+*/%~!&|^-])\Z')


# A direct generator body reserves yield. Read an ordinary header without
# consuming expressions or quoted text in parameters.
_GENERATOR_HEADER = (
    r"""(?<![\w$.])function\s*\*\s*(?:[$\w]+\s*)?\([^(){}'"/`]*\)\s*\{"""
)
_GENERATOR_EVENT = re.compile('(?P<generator>' + _GENERATOR_HEADER + r""")|[/`'"{}]""")
_AMBIGUOUS_YIELD = re.compile(rf'(?:\byield\s*\*\s*|>\s*[{_LINE_TERMINATORS}]\s*)yield\b')


def mark_keyword_slashes(source, *, jsx=False):
    """Find-jump scan: C-level seeks between quotes, templates and slashes."""
    marks = []
    # The JSX tokenizer identifies element text after this scan has run.
    comments = None if jsx else {}
    literal_ends = None if jsx else set()
    generator_events = _GENERATOR_EVENT.search if (
        not jsx and _AMBIGUOUS_YIELD.search(source) is not None) else None
    braces = 0
    generator_bodies = []
    n = len(source)
    i = 0
    find = source.find
    while i < n:
        if generator_events:
            event = generator_events(source, i)
            if event is None:
                break
            i = event.start()
            if event.lastgroup == 'generator':
                prev, _ = _before_comments(source, _skip_ws_back(source, i - 1), comments)
                if prev < 0 or source[prev] not in '.#':
                    braces += 1
                    generator_bodies.append(braces)
                    i = event.end()
                else:
                    i += len('function')
                continue
        else:
            j_slash = find('/', i)
            j_dq = find('"', i)
            j_sq = find("'", i)
            j_tmpl = find('`', i)
            candidates = [j for j in (j_slash, j_dq, j_sq, j_tmpl) if j >= 0]
            if not candidates:
                break
            i = min(candidates)
        ch = source[i]
        if ch == '{':
            braces += 1
            i += 1
            continue
        if ch == '}':
            if generator_bodies and generator_bodies[-1] == braces:
                generator_bodies.pop()
            braces -= 1
            i += 1
            continue
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
            i = _skip_template(source, i, n, marks, comments, literal_ends)
            continue
        if i + 1 < n and source[i + 1] in '/*':
            i = _read_comment(source, i, comments)
            continue
        direct_generator = bool(generator_bodies and generator_bodies[-1] == braces)
        i = _consider_slash(source, i, marks, comments, literal_ends, direct_generator=direct_generator)
    return _apply_marks(source, marks)


def _read_comment(source, i, comments):
    end = _COMMENT.match(source, i).end()
    if comments is not None:
        comments[_skip_ws_back(source, end - 1)] = i
    return end


def _before_comments(source, i, comments):
    line_comment = False
    while i in comments:
        start = comments[i]
        line_comment = line_comment or source[start + 1] == '/'
        i = _skip_ws_back(source, start - 1)
    return i, line_comment


def _consider_slash(source, i, marks, comments, literal_ends, *, direct_generator=False):
    matched = _REGEX_LITERAL_RE.match(source, i)
    if matched is None:
        return i + 1
    if comments is None:
        _mark_jsx_slash(source, i, marks)
        return matched.end()
    before = _skip_ws_back(source, i - 1)
    prev, line_comment = _before_comments(source, before, comments)
    commented = prev != before
    if commented:
        # A TypeScript non-null assertion leaves a completed operand.
        while prev >= 0 and source[prev] == '!':
            prev, skipped_line = _before_comments(source, _skip_ws_back(source, prev - 1), comments)
            line_comment = line_comment or skipped_line
    if prev < 0 or source[prev] in _EXPRESSION_PUNCTUATION:
        if commented:
            marks.append(i)
        literal_ends.add(matched.end() - 1)
        return matched.end()
    if prev >= 1 and source[prev] == '>' and source[prev - 1] == '=':
        marks.append(i)
        literal_ends.add(matched.end() - 1)
        return matched.end()
    word, before_word = _word_before(source, prev)
    if word in _EXPRESSION_KEYWORDS:
        prefix, _ = _before_comments(source, _skip_ws_back(source, before_word), comments)
        member = prefix >= 0 and (source[prefix] == '#'
                                  or source[prefix] == '.' and _is_property_before(source, prefix))
        if not member and (
                word != 'yield' or direct_generator or
                _yield_starts_expression(source, prefix, before_word, line_comment, comments, literal_ends)):
            marks.append(i)
            literal_ends.add(matched.end() - 1)
            return matched.end()
    # A same-line operand makes this division. Resume at its right operand
    # so the normal scan can keep strings and comments in their own spans.
    operand = source[prev]
    if (word not in ('await', 'default', 'extends', 'yield') and (operand.isalnum() or operand in '_$]`"\'')
            and _LINE_BREAK.search(source, prev + 1, i) is None):
        return i + 1
    return matched.end()


def _yield_starts_expression(source, prefix, before_word, line_comment, comments, literal_ends):
    """A classic script can use yield as the operand of a division."""
    if line_comment:
        return False
    if prefix < 0:
        return True
    end = prefix + 1
    new_line = _LINE_BREAK.search(source, end, before_word + 1) is not None
    if new_line and (prefix in literal_ends or source[max(0, end - 2):end] in ('++', '--')):
        return True
    if _OPERAND_OPERATOR.search(source, max(0, end - 2), end):
        return False
    word, before_operator = _word_before(source, prefix)
    if word in ('in', 'instanceof'):
        receiver, _ = _before_comments(source, _skip_ws_back(source, before_operator), comments)
        return receiver >= 0 and source[receiver] in '.#'
    return new_line or word not in ('typeof', 'void', 'delete', 'new', 'await')


def _mark_jsx_slash(source, i, marks):
    """Use the keyword and arrow rules for the JSX source pre-scan."""
    prev = _skip_ws_and_comments_back(source, i - 1)
    if prev >= 1 and source[prev - 1:prev + 1] == '=>':
        marks.append(i)
        return
    word, before_word = _word_before(source, prev, unicode_names=False)
    if word in _EXPRESSION_KEYWORDS:
        if not _is_property_before(source, before_word):
            marks.append(i)


def _skip_template(source, i, n, marks, comments, literal_ends):
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
                        source, i, n, marks, comments, literal_ends)
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
                    i = _read_comment(source, i, comments)
                    continue
                if ch == '/':
                    i = _consider_slash(source, i, marks, comments, literal_ends)
                    continue
                i += 1
            continue
        i += 1
    return i
