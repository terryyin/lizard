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


# The scope scan only runs for ambiguous yield spellings.
_SCOPE_TRIVIA = rf'(?:[\s\ufeff]|/\*(?:[^*]|\*(?!/))*\*/|//[^{_LINE_TERMINATORS}]*[{_LINE_TERMINATORS}])*'
_FUNCTION_HEAD = (
    rf'(?<![$\w.])function\b{_SCOPE_TRIVIA}'
    rf'(?:(?P<star>\*){_SCOPE_TRIVIA})?(?:[$\w]+{_SCOPE_TRIVIA})?(?=\()'
)
_GENERATOR_EVENT = re.compile('(?P<function>' + _FUNCTION_HEAD + r""")|=>|[/`'"{}()[\],;]""")
_BODY_OPEN = re.compile(_SCOPE_TRIVIA + r'\{')
_ANNOTATED_BODY_OPEN = re.compile(rf'{_SCOPE_TRIVIA}:{_SCOPE_TRIVIA}[$\w]+{_SCOPE_TRIVIA}\{{')
_CONTROL_HEADS = frozenset(('if', 'while', 'for', 'with', 'switch', 'catch'))
_AMBIGUOUS_YIELD = re.compile(rf'(?:\byield\s*\*\s*|>\s*[{_LINE_TERMINATORS}]\s*)yield\b')


class _FunctionScopes:
    """Track yield's function scope while the slash scan skips opaque text."""

    def __init__(self, source):
        self.source = source
        self.depth = 0
        self.braces = []
        self.parameters = []
        self.arrows = []
        self.next_parameters = {}
        self.body_starts = {}

    def in_generator(self):
        return bool(self.braces and self.braces[-1] and not self.arrows)

    def read(self, event, comments):
        """Update scope. Return True for events consumed in full."""
        token = event.group()
        pos = event.start()
        if event.lastgroup == 'function':
            prefix, _ = _before_comments(self.source, _skip_ws_back(self.source, pos - 1), comments)
            if prefix < 0 or self.source[prefix] not in '.#':
                self.next_parameters[event.end()] = event.group('star') is not None
            return True
        if token == '=>':
            if not self._expect_body(event.end(), False):
                self.arrows.append(self.depth)
            return True
        if token in '([{':
            if token == '(':
                self.parameters.append(self._parameter_role(pos, comments))
            elif token == '{':
                self.braces.append(self.body_starts.pop(pos, self.in_generator()))
            self.depth += 1
        elif token in ')]}':
            if token == ')' and self.parameters:
                generator, method = self.parameters.pop()
                if generator is not None:
                    self._expect_body(event.end(), generator, method=method, annotated=True)
            elif token == '}' and self.braces:
                self.braces.pop()
            self.depth -= 1
            self._end_arrows(self.depth + 1)
        elif token in ',;':
            self._end_arrows(self.depth)
        return False

    def _parameter_role(self, pos, comments):
        if pos in self.next_parameters:
            return self.next_parameters.pop(pos), False
        prev, _ = _before_comments(self.source, _skip_ws_back(self.source, pos - 1), comments)
        if prev < 0:
            return None, False
        word, before_word = _word_before(self.source, prev)
        prefix, _ = _before_comments(self.source, _skip_ws_back(self.source, before_word), comments)
        if word in _CONTROL_HEADS:
            if prefix < 0 or self.source[prefix] not in '.#':
                self._end_arrows(self.depth)
            return None, False
        # A named method body can start after an object opener or separator.
        if word and prefix >= 0 and self.source[prefix] in '{,':
            return False, True
        return None, False

    def _expect_body(self, pos, generator, *, method=False, annotated=False):
        opening = _BODY_OPEN.match(self.source, pos)
        if opening is None and annotated:
            opening = _ANNOTATED_BODY_OPEN.match(self.source, pos)
        if opening is None or method and _LINE_BREAK.search(self.source, pos, opening.end()):
            return False
        self.body_starts[opening.end() - 1] = generator
        return True

    def _end_arrows(self, depth):
        while self.arrows and self.arrows[-1] >= depth:
            self.arrows.pop()


def mark_keyword_slashes(source, *, jsx=False):
    """Find-jump scan: C-level seeks between quotes, templates and slashes."""
    marks = []
    # The JSX tokenizer identifies element text after this scan has run.
    comments = None if jsx else {}
    literal_ends = None if jsx else set()
    generator_events = _GENERATOR_EVENT.search if (
        not jsx and _AMBIGUOUS_YIELD.search(source) is not None) else None
    scopes = _FunctionScopes(source) if generator_events else None
    n = len(source)
    i = 0
    find = source.find
    while i < n:
        if generator_events:
            event = generator_events(source, i)
            if event is None:
                break
            i = event.start()
            if scopes.read(event, comments):
                i = event.end()
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
        if ch in '(){}[],;':
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
        in_generator = scopes is not None and scopes.in_generator()
        i = _consider_slash(source, i, marks, comments, literal_ends, in_generator=in_generator)
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


def _consider_slash(source, i, marks, comments, literal_ends, *, in_generator=False):
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
                word != 'yield' or in_generator or
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
