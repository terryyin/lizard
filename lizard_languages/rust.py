'''
Language parser for Rust lang
'''

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

    def __init__(self, context):
        super().__init__(context)
        self.parallel_states = [RustStates(context)]

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        addition = r"|(?:'\w+\b)"  # lifetimes, labels
        return CodeReader.generate_tokens(source_code, addition, token_class)


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
