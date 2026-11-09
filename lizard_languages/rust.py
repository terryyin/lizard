'''
Language parser for Rust lang
'''

import re

from .code_reader import CodeReader
from .clike import CCppCommentsMixin
from .golike import GoLikeStates


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
        return CodeReader.generate_tokens(
            _flatten_nested_comments(source_code), addition, token_class)

    def preprocess(self, tokens):
        """Split the `||` of a closure without parameters, `|| x`, into two
        `|`, as `|a| x` has, so that it is not counted as a logical or."""
        openers = self._CLOSURE_OPENERS
        last = None
        for token in tokens:
            if token.isspace():
                if token == '\n':
                    yield token
                continue
            if token == '||' and last in openers:
                yield '|'
                yield '|'
            else:
                yield token
            if token[0] != '/':  # comments do not end an expression
                last = token


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
