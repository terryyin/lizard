'''Decide which slashes open a JS/Ruby regex literal versus division.'''

import re

# One literal grammar shared with balanced template scanning; context decides
# whether a slash opens that literal or remains division.
REGEX_LITERAL = (r"/(?![/*])(?:\\[^\r\n]|\[(?:\\[^\r\n]|[^\\\]\r\n])*\]"
                 r"|[^/\\[\r\n])+/[a-z]*")
# Pre-token marker so REGEX_TOKEN needs no keyword lookbehind (tokens_in_span).
REGEX_START_MARKER = "\ue000"
REGEX_TOKEN = r"\s*" + REGEX_START_MARKER + REGEX_LITERAL[1:]
_REGEX_LITERAL_RE = re.compile(REGEX_LITERAL)
# Expression-start keywords; contextual of/await use structure, not this set.
_EXPRESSION_KEYWORDS = frozenset({
    'return', 'typeof', 'instanceof', 'in', 'new', 'delete', 'void',
    'throw', 'case', 'do', 'else', 'yield',
})
_EXPR_OPENING_PUNCT = frozenset('=,({[?:!&|;')  # historic REGEX_TOKEN openers
_LINE_ENDS = '\r\n\u2028\u2029'


def mark_regex_literal_starts(source, language='js'):
    """Mark regex-open slashes; leave division alone.

    Tracks expression starts; completed operands/members stay division.
    ``of`` / ``await`` follow for-header / async structure.
    """
    if '/' not in source:
        return source
    return _RegexLiteralContext(source, language).marked_source()


class _RegexLiteralContext:
    def __init__(self, source, language):
        self.source, self.js_family = source, language != 'ruby'
        self.n, self.i, self.chars = len(source), 0, None
        self.can_regex, self.expect_property = True, False
        self.paren_depth, self.for_headers, self.async_nesting = 0, [], 0
        self.pending_async = self.pending_for = False

    def marked_source(self):
        while self.i < self.n:
            self.advance()
        return self.source if self.chars is None else ''.join(self.chars)

    def after_operand(self, can_regex=False, expect_property=False):
        self.can_regex, self.expect_property = can_regex, expect_property
        self.pending_async = False

    def skip_string(self, quote):
        self.i += 1
        while self.i < self.n:
            ch = self.source[self.i]
            if ch == '\\':
                self.i += 2
                continue
            if ch == quote:
                self.i += 1
                return
            if self.js_family and ch in _LINE_ENDS:
                return
            self.i += 1

    def scan_expression(self, stop_below):
        brace_depth = stop_below
        self.after_operand(can_regex=True)
        while self.i < self.n:
            ch = self.source[self.i]
            if ch == '{':
                brace_depth += 1
                self.i += 1
                self.after_operand(can_regex=True)
            elif ch == '}':
                brace_depth -= 1
                self.i += 1
                self.after_operand()
                if brace_depth < stop_below:
                    return
            else:
                self.advance()

    def scan_template(self):
        self.i += 1
        while self.i < self.n:
            ch = self.source[self.i]
            if ch == '\\':
                self.i += 2
            elif ch == '`':
                self.i += 1
                self.after_operand()
                return
            elif ch == '$' and self.i + 1 < self.n and self.source[self.i + 1] == '{':
                self.i += 2
                self.scan_expression(1)
            else:
                self.i += 1
        self.after_operand()

    def read_word(self):
        start = self.i
        self.i += 1
        while self.i < self.n and (
                self.source[self.i].isalnum() or self.source[self.i] in '_$'):
            self.i += 1
        return self.source[start:self.i]

    def after_word(self, word):
        if self.expect_property:
            self.after_operand()
        elif word == 'async' and self.js_family:
            self.pending_async = True
            self.can_regex = False
        elif word == 'function':
            if self.pending_async:
                self.async_nesting += 1
                self.pending_async = False
            self.can_regex = False
        elif word == 'for' and self.js_family:
            self.pending_for = True
            self.after_operand()
        elif (word == 'of' and self.js_family and self.for_headers
              and self.paren_depth == self.for_headers[-1]):
            self.for_headers.pop()
            self.after_operand(can_regex=True)
        elif word == 'await' and self.js_family and self.async_nesting > 0:
            self.after_operand(can_regex=True)
        elif word in _EXPRESSION_KEYWORDS:
            self.after_operand(can_regex=True)
        else:
            self.after_operand()

    def advance_comment_or_regex(self, ch):
        source, i, n = self.source, self.i, self.n
        if self.js_family and ch == '/' and i + 1 < n and source[i + 1] == '/':
            self.i += 2
            while self.i < n and self.source[self.i] not in _LINE_ENDS:
                self.i += 1
            return True
        if self.js_family and ch == '/' and i + 1 < n and source[i + 1] == '*':
            self.i += 2
            while self.i + 1 < n and not (
                    self.source[self.i] == '*' and self.source[self.i + 1] == '/'):
                self.i += 1
            self.i = min(self.i + 2, n)
            return True
        if not self.js_family and ch == '#':
            self.i += 1
            while self.i < n and self.source[self.i] not in '\r\n':
                self.i += 1
            return True
        if ch == '/' and self.can_regex:
            match = _REGEX_LITERAL_RE.match(source, i)
            if match:
                if self.chars is None:
                    self.chars = list(source)
                self.chars[i] = REGEX_START_MARKER
                self.i = match.end()
                self.after_operand()
                return True
        return False

    def advance(self):
        ch = self.source[self.i]
        source, i = self.source, self.i

        if ch in ' \t\v\f' + _LINE_ENDS:
            self.i += 1
            return
        if self.advance_comment_or_regex(ch):
            return
        if ch in '"\'':
            self.skip_string(ch)
            self.after_operand()
            return
        if ch == '`' and self.js_family:
            self.scan_template()
            self.pending_async = False
            return
        if source.startswith('=>', i):
            self.i += 2
            self.after_operand(can_regex=True)
            return
        if source.startswith('?.', i):
            self.i += 2
            self.after_operand(expect_property=True)
            return
        if source.startswith('++', i) or source.startswith('--', i):
            self.i += 2
            self.after_operand()
            return
        if ch.isalpha() or ch in '_$':
            self.after_word(self.read_word())
            return
        if ch.isdigit():
            self.i += 1
            while self.i < self.n and (
                    self.source[self.i].isalnum() or self.source[self.i] in '._'):
                self.i += 1
            self.after_operand()
            return
        if ch == '(':
            if self.pending_for:
                self.for_headers.append(self.paren_depth + 1)
                self.pending_for = False
            if self.pending_async:
                self.async_nesting += 1
                self.pending_async = False
            self.paren_depth += 1
            self.i += 1
            self.can_regex = True
            self.expect_property = False
            return
        if ch == ')':
            self.paren_depth = max(0, self.paren_depth - 1)
            while self.for_headers and self.for_headers[-1] > self.paren_depth:
                self.for_headers.pop()
            self.i += 1
            self.after_operand()
            return
        if ch == '.':
            self.i += 1
            self.after_operand(expect_property=True)
            return
        if ch in _EXPR_OPENING_PUNCT:
            self.i += 1
            self.after_operand(can_regex=True)
            return
        if ch == '}':
            if self.async_nesting and self.paren_depth == 0:
                self.async_nesting = max(0, self.async_nesting - 1)
            self.i += 1
            self.after_operand()
            return
        if ch in '];':
            self.i += 1
            self.after_operand(can_regex=(ch == ';'))
            return
        # Historic openers only; <> is not regex context (JSX "</...>" stays safe).
        if ch in '+-*%~^@':
            self.i += 1
            self.after_operand(can_regex=True)
            return
        self.i += 1
        self.after_operand()
