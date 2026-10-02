'''
Language parser for Go lang
'''

from .code_reader import CodeReader
from .clike import CCppCommentsMixin
from .golike import GoLikeStates


class GoReader(CodeReader, CCppCommentsMixin):
    # pylint: disable=R0903

    ext = ['go']
    language_names = ['go']

    def __init__(self, context):
        super(GoReader, self).__init__(context)
        self.parallel_states = [GoStates(context)]

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        addition = addition + r"|`[^`]*`"  # Add support for backtick-quoted strings
        return CodeReader.generate_tokens(source_code, addition, token_class)

    def __call__(self, tokens, reader):
        self.context = reader.context
        for token in tokens:
            # Skip counting ? in backtick-quoted strings
            if token.startswith('`') and token.endswith('`'):
                for state in self.parallel_states:
                    state(token)
                yield token
                continue

            # For non-backtick tokens, process normally
            for state in self.parallel_states:
                state(token)
            yield token
        for state in self.parallel_states:
            state.statemachine_before_return()
        self.eof()


# Tokens after which `func` starts a literal (an expression), not a declaration.
_LITERAL_PRECEDERS = frozenset(("=", ",", "(", ":", "{", "[", "return"))
# Tokens after which `func` starts a func type inside a larger type.
_FUNC_TYPE_PRECEDERS = frozenset(("]", "chan", "*"))
# Tokens that start a package-level declaration.
_DECLARATION_KEYWORDS = frozenset(("const", "func", "import", "type", "var"))


class GoStates(GoLikeStates):  # pylint: disable=R0903
    """Go states that parse `func` in expression position as a literal,
    `var name func(...)` as a func type, and `.(type)` as a type switch."""

    def __init__(self, context):
        super(GoStates, self).__init__(context)
        self._func_type_depth = 0
        self._literal_name = ""
        self._token_before_last = None
        self._typed_var_name = ""

    def __call__(self, token, reader=None):
        before = self.last_token
        result = super(GoStates, self).__call__(token, reader)
        self._token_before_last = before
        return result

    def statemachine_clone(self):
        # Clones start right after a `{`, which can precede a literal.
        clone = super(GoStates, self).statemachine_clone()
        clone.last_token = "{"
        return clone

    def _literal_parameters(self, token):
        # Only open the literal once its `(` arrives, so malformed source
        # such as a bare `= func` does not swallow the functions after it.
        if token == "(":
            self.context.push_new_function(self._literal_name)
            self.next(self._function_dec, token)
        else:
            self.next(self._state_global, token)

    def _func_type(self, token):
        # Newlines never reach the parser, so the type ends at the first `=`
        # or declaration keyword outside its brackets, or at the `{` of a
        # composite literal such as `[]func(){...}`.
        if token == "{" and self._func_type_depth == 0 \
                and self.last_token not in ("interface", "struct"):
            self.next(self._state_global, token)
        elif token in ("(", "[", "{"):
            self._func_type_depth += 1
        elif token in (")", "]", "}"):
            self._func_type_depth -= 1
            if self._func_type_depth < 0:
                self.next(self._state_global, token)
        elif self._func_type_depth == 0 and token == "=":
            self._state = self._state_global
        elif self._func_type_depth == 0 and token in _DECLARATION_KEYWORDS:
            self._typed_var_name = ""
            self.next(self._state_global, token)

    def _state_global(self, token):
        # The name from `var handler func() =` only applies right after `=`.
        typed_var_name, self._typed_var_name = self._typed_var_name, ""
        last = self.last_token
        if token == "func" and self._token_before_last == "var" \
                and last and last.isidentifier():
            self._typed_var_name = last
            self._func_type_depth = 0
            self._state = self._func_type
        elif token == "func" and last in _FUNC_TYPE_PRECEDERS:
            # `[]func()`, `map[K]func()`, `chan func()`, `*func()` are types.
            self._func_type_depth = 0
            self._state = self._func_type
        elif token == "func" and last in _LITERAL_PRECEDERS:
            # `var handler = func(...)` is reported as `handler`; other
            # literals stay anonymous.
            name = (typed_var_name or self._token_before_last) \
                if last == "=" else ""
            self._literal_name = name if name and name.isidentifier() else ""
            self._state = self._literal_parameters
        elif token == "type" and last == "(":
            # `x.(type)` in a type switch, not a type declaration.
            pass
        else:
            super(GoStates, self)._state_global(token)
