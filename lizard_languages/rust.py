'''
Language parser for Rust lang
'''

import re
from collections import deque

from .code_reader import CodeReader
from .clike import CCppCommentsMixin
from .golike import GoLikeStates
from .token_pattern import compiled_token_pattern, tokens_from_matches


# Rust punctuation is ASCII. U+200E and U+200F are whitespace in Rust.
_NON_ASCII_IDENTIFIER_CHAR = r'[^\x00-\x7f\s\u200e\u200f]'
_RAW_STRING_TOKEN = (
    # Reject prefixes within names and suffixes, including raw C suffixes.
    r'''|(?=[br])(?<![A-Za-z0-9_'"#])(?<!''' + _NON_ASCII_IDENTIFIER_CHAR + r')'
    r'b?r(?P<rust_raw_hashes>#{0,255})"[^"]*'
    r'(?:"(?!(?P=rust_raw_hashes))[^"]*)*"(?P=rust_raw_hashes)'
    r'(?:(?:[A-Za-z_]|' + _NON_ASCII_IDENTIFIER_CHAR +
    r')(?:[A-Za-z_0-9]|' + _NON_ASCII_IDENTIFIER_CHAR + r')*)?'
)


class RustReader(CodeReader, CCppCommentsMixin):
    # pylint: disable=R0903

    ext = ['rs']
    language_names = ['rust']

    # Separated condition categories
    _control_flow_keywords = {'if', 'for', 'while', 'catch', 'where'}
    _logical_operators = {'&&', '||'}
    _case_keywords = set()  # Rust match arms are counted via `=>` in RustStates
    # Note: '?' in Rust is the error propagation operator, not ternary
    _ternary_operators = {'?'}

    # After one of these, `||` opens a closure without parameters.
    _CLOSURE_OPENERS = {'(', '[', '{', ',', ';', ':', '=', '=>', '->',
                        '&&', '||', '|', 'move', 'async', 'return'}

    def __init__(self, context):
        super().__init__(context)
        self.parallel_states = [RustStates(context)]

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        addition = (
            # char literals, before lifetimes, which also start with `'`
            r"|(?:'(?:\\(?:x[0-9a-fA-F]{2}|u\{[0-9a-fA-F]+\}|.)|[^'\\\n])')"
            r"|(?:'\w+\b)")  # lifetimes, labels
        if 'r"' in source_code or 'r#' in source_code:
            addition += _RAW_STRING_TOKEN
            if token_class is None:
                token_class = _rust_token_text
        source_code = _flatten_nested_comments(source_code)
        if 'quote' not in source_code:
            return CodeReader.generate_tokens(source_code, addition, token_class)
        return _quote_tokens(source_code, addition, token_class)

    def preprocess(self, tokens):
        """Normalize an empty closure's `||` to one non-logical pipe token."""
        openers = self._CLOSURE_OPENERS
        last = None
        for token in tokens:
            if token.isspace():
                if token == '\n':
                    yield token
                continue
            if token == '||' and last in openers:
                yield '|'
            else:
                yield token
            if token[0] != '/':  # comments do not end an expression
                last = token


def _rust_token_text(match):
    token = match.group(0)
    if match.lastgroup == 'rust_raw_hashes':
        # Extensions identify string tokens by the first quote.
        return '"' + token
    return token


class RustStates(GoLikeStates):  # pylint: disable=R0903
    FUNC_KEYWORD = 'fn'

    def __init__(self, context, in_match_arms=False):
        super().__init__(context)
        self._in_match_arms = in_match_arms
        self._seen_match_arm = False
        self._match_subject_nesting = 0
        # Paren depth of each `if` still waiting for its body, innermost last.
        self._if_body_depths = []
        self._else_pending = False
        self._paren_depth = 0

    def _state_global(self, token):
        if token == '=>':
            if self._in_match_arms:
                if self._seen_match_arm:
                    self.context.add_condition()
                self._seen_match_arm = True
            return
        if token == 'match':
            self._match_subject_nesting = 0
            self._state = self._match_subject
            return
        if self._count_let_else(token):
            return
        super()._state_global(token)

    def _count_let_else(self, token):
        """Count `let ... else`. The `else` of an `if` is not another condition.

        Brace bodies are separate state machines, so an `if` and its `else`
        are both seen here with the body omitted. The body is the `{` that
        returns to the parenthesis depth where that `if` began.
        """
        if token != 'else':
            self._else_pending = False
        if token in '([':
            self._paren_depth += 1
        elif token in ')]' and self._paren_depth:
            self._paren_depth -= 1
        if token == 'else':
            if not self._else_pending:
                self.context.add_condition()
            self._else_pending = False
            return True
        if token == 'if':
            self._if_body_depths.append(self._paren_depth)
            return False
        if (token == '{' and self._if_body_depths
                and self._paren_depth == self._if_body_depths[-1]):
            self._if_body_depths.pop()
            self.sub_state(self.statemachine_clone(), self._if_body_closed)
            return True
        return False

    def _if_body_closed(self):
        self._else_pending = True

    def _match_subject(self, token):
        if token in '([':
            self._match_subject_nesting += 1
            return
        if token in ')]':
            if self._match_subject_nesting:
                self._match_subject_nesting -= 1
            return
        if token != '{' or self._match_subject_nesting:
            return
        self.sub_state(
            RustStates(self.context, in_match_arms=True),
            self._end_match)

    def _end_match(self):
        self.next(self._state_global)


_QUOTE_MACROS = {'quote', 'quote_spanned'}
_TOKEN_TREE_DEPTH = {'(': 1, '[': 1, '{': 1, ')': -1, ']': -1, '}': -1}
# These prefixes can remain split into tokens. Their text can contain braces.
_UNRECOGNIZED_RAW_START = re.compile(r'(?:\bcr|#b?r)#*"')


def _quote_tokens(source, addition, token_class):
    """Keep the token count of quote input without parsing it as Rust code."""
    matches = compiled_token_pattern(addition).finditer(source)
    raw_starts = {match.start() for match in _UNRECOGNIZED_RAW_START.finditer(source)}
    lookahead = _QuoteLookahead(matches, raw_starts) if raw_starts else None

    depth = 0
    span_expression = False
    after_name = after_bang = None
    previous = before_previous = None
    scope_depth = 0
    local_macros = []

    def read_token(match):
        nonlocal depth, span_expression, after_name, after_bang
        nonlocal previous, before_previous, scope_depth
        token = match.group()
        text = token if token_class is None else token_class(match)
        if token.isspace() or token.startswith(('//', '/*')):
            return text
        if depth:
            depth += _TOKEN_TREE_DEPTH.get(token, 0)
            if span_expression and depth == 1 and token == '=>':
                span_expression = False
            return text if span_expression and depth else _quote_data(text)

        macro = after_bang
        after_bang = after_name if token == '!' else None
        if macro == 'macro_rules' and token in _QUOTE_MACROS:
            local_macros.append((token, scope_depth))
        is_macro = token == 'macro_rules' or (
            token in _QUOTE_MACROS and _quote_name_visible(token, previous, before_previous, local_macros))
        after_name = token if is_macro else None
        before_previous, previous = previous, token

        if macro in _QUOTE_MACROS and _TOKEN_TREE_DEPTH.get(token) == 1 and (
                lookahead is None or not lookahead.has_ambiguous_raw(match)):
            depth = 1
            span_expression = macro == 'quote_spanned'
            return _quote_data(text)
        if token == '{':
            scope_depth += 1
        elif token == '}':
            while local_macros and local_macros[-1][1] == scope_depth:
                local_macros.pop()
            scope_depth -= 1
        return text

    return tokens_from_matches(matches if lookahead is None else lookahead, read_token)


def _quote_name_visible(token, previous, before_previous, local_macros):
    if previous == '::':
        return before_previous == 'quote'
    return all(name != token for name, _ in local_macros)


def _quote_data(token):
    # Like a string token, quoted text supplies data to the macro.
    return token if token.startswith(('"', "'")) else '"' + token


class _QuoteLookahead:
    """Look for an ambiguous literal inside this invocation using the same matches."""

    def __init__(self, matches, raw_starts):
        self._matches = matches
        self._pending = deque()
        self._raw_starts = raw_starts
        self._last_raw_start = max(raw_starts)

    def __iter__(self):
        return self

    def __next__(self):
        return self._pending.popleft() if self._pending else next(self._matches)

    def has_ambiguous_raw(self, opener):
        if opener.end() > self._last_raw_start:
            return False
        depth = 1
        for match in self._peek():
            start = match.start()
            if start in self._raw_starts:
                return True
            depth += _TOKEN_TREE_DEPTH.get(match.group(), 0)
            if not depth or start >= self._last_raw_start:
                break
        return False

    def _peek(self):
        yield from self._pending
        for match in self._matches:
            self._pending.append(match)
            yield match


_CHAR_LITERAL = re.compile(
    r"'(?:\\(?:x[0-9a-fA-F]{2}|u\{[0-9a-fA-F]+\}|.)|[^'\\\n])'")
_RAW_STRING = re.compile(r'b?r(#*)"')


def _flatten_nested_comments(source):
    """Rust block comments nest: `/* a /* b */ c */` is one comment.
    The shared tokenizer ends a block comment at its first `*/`, so a nested
    one (e.g. `src/**/foo.rs` in a doc comment) leaks the rest of the comment
    into the code. Break the inner `*/`s so the comment is read whole.
    Strings, raw strings, char literals and line comments are skipped, so a
    `/*` inside them does not open a comment."""
    if '/*' not in source:
        return source
    out = []
    i, n = 0, len(source)
    while i < n:
        two = source[i:i + 2]
        if two == '//':
            end = source.find('\n', i)
            end = n if end < 0 else end
        elif two == '/*':
            depth, j = 1, i + 2
            while j < n and depth:
                if source.startswith('/*', j):
                    depth, j = depth + 1, j + 2
                elif source.startswith('*/', j):
                    depth, j = depth - 1, j + 2
                else:
                    j += 1
            if depth:  # unterminated: leave it to the tokenizer
                out.append(source[i:])
                break
            inner = source[i + 2:j - 2]
            out.append('/*' + inner.replace('*/', '* /') + '*/')
            i = j
            continue
        elif source[i] == '"' or (
                source[i] in 'br' and (i == 0 or not (
                    source[i - 1].isalnum() or source[i - 1] == '_'))
                and _RAW_STRING.match(source, i)):
            raw = _RAW_STRING.match(source, i) if source[i] != '"' else None
            if raw:
                close = '"' + raw.group(1)
                end = source.find(close, raw.end())
                end = n if end < 0 else end + len(close)
            else:
                j = i + 1
                while j < n and source[j] != '"':
                    j += 2 if source[j] == '\\' else 1
                end = min(j + 1, n)
        elif source[i] == "'":
            match = _CHAR_LITERAL.match(source, i)
            end = match.end() if match else i + 1
        else:
            end = i + 1
        out.append(source[i:end])
        i = end
    return ''.join(out)
