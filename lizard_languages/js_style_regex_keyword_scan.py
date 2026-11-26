'''Find regex openings after keywords, arrows and comments.'''

import re

from .js_style_regex_context import (
    _BASIC_WHITESPACE,
    _EXPRESSION_KEYWORDS,
    _EXPRESSION_PUNCTUATION,
    _LINE_TERMINATORS,
    _REGEX_LITERAL_RE,
    _STRING_LINE_ENDS,
    _apply_marks,
    _skip_ws_back,
    _word_before,
)

_COMMENT = re.compile(rf'//[^{_LINE_TERMINATORS}]*|/\*.*?(?:\*/|\Z)', re.S)
_LINE_BREAK = re.compile(rf'[{_LINE_TERMINATORS}]')
# Match an operator before an operand. Compound assignments and arrows can
# precede yield, as can a single question mark in a conditional expression.
_OPERAND_OPERATOR = re.compile(r'(?:[=!]=|(?<!<)<=|(?<!>)>=|(?<!=)>|\?\?|[<+*/%~!&|^-])\Z')


_AFTER_PAREN = re.compile(r'\)\s*/(?![/*])')
_PAREN_EVENTS = re.compile(r"""[/()`"']""")
_CONTROL_HEADERS = frozenset(('if', 'while', 'for', 'with'))


def mark_keyword_slashes(source, *, jsx=False):
    """Find-jump scan: C-level seeks between quotes, templates and slashes."""
    marks = []
    # The JSX tokenizer identifies element text after this scan has run.
    comments = None if jsx else {}
    literal_ends = None if jsx else set()
    roles = []
    closing = None
    scan_parens = not jsx and _AFTER_PAREN.search(source) is not None
    n = len(source)
    i = 0
    find = source.find
    while i < n:
        if scan_parens:
            event = _PAREN_EVENTS.search(source, i)
            if event is None:
                break
            i = event.start()
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
        if ch == '(':
            prev, _ = _before_comments(source, _skip_ws_back(source, i - 1), comments)
            word, before = _word_before(source, prev)
            control = word in _CONTROL_HEADERS
            if control:
                receiver, _ = _before_comments(source, _skip_ws_back(source, before), comments)
                control = receiver < 0 or source[receiver] not in '.#'
            roles.append(control)
            i += 1
            continue
        if ch == ')':
            if roles:
                closing = i, roles.pop()
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
        i = _consider_slash(source, i, marks, comments, literal_ends, closing)
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


def _consider_slash(source, i, marks, comments, literal_ends, closing=None):
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
    if closing is not None and prev == closing[0]:
        # A control header starts a statement. A call or group ends an operand.
        if not closing[1]:
            return i + 1
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
        if (prefix < 0 or source[prefix] not in '.#') and (
                word != 'yield' or
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
    prev = _skip_jsx_trivia(source, i - 1)
    if prev >= 1 and source[prev - 1:prev + 1] == '=>':
        marks.append(i)
        return
    word, before_word = _word_before(source, prev, unicode_names=False)
    if word in _EXPRESSION_KEYWORDS:
        prefix = _skip_jsx_trivia(source, before_word)
        if prefix < 0 or source[prefix] != '.':
            marks.append(i)


def _skip_jsx_trivia(source, i):
    while i >= 0:
        ch = source[i]
        if ch in _BASIC_WHITESPACE:
            i -= 1
        elif ch == '/' and i >= 1 and source[i - 1] == '*':
            i -= 2
            while i >= 1:
                if source[i - 1] == '/' and source[i] == '*':
                    i -= 2
                    break
                i -= 1
        else:
            break
    return i


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
