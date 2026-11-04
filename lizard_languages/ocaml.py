'''
Language parser for OCaml
'''

import re
from .code_reader import CodeReader, CodeStateMachine
from .ocaml_tokens import generate_tokens as generate_ocaml_tokens


class OCamlReader(CodeReader):
    # pylint: disable=R0903

    ext = ['ml']
    language_names = ['ocaml']

    _control_flow_keywords = {'if', 'for', 'while', 'when'}
    _logical_operators = {'&&', '||'}
    _case_keywords = set()
    # '?' marks optional arguments in OCaml, not a ternary
    _ternary_operators = set()

    def __init__(self, context):
        super().__init__(context)
        self.parallel_states = [OCamlStates(context)]

    @staticmethod
    def get_comment_from_token(token):
        if token.startswith('(*'):
            return token[2:-2]
        return None

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        return generate_ocaml_tokens(source_code)


class _ArmScope:
    '''An open `match` / `try` / `function` at the bracket depth where
    it began. It moves from its subject (before `with`) to a pattern
    (before an arm's `->`) to an arm body (until the next `|`). The first
    `match` / `function` arm is the default path; every `try` handler
    arm is a decision.'''
    # pylint: disable=R0903

    def __init__(self, depth, phase, counts_first_arm):
        self.depth, self.phase = depth, phase
        self.counts_arm = counts_first_arm

    def arm_token(self, token):
        '''Advance on `|` or `->`; return whether it adds a decision.'''
        if self.phase == 'subject':
            return False
        if token == '|':
            self.phase = 'pattern'
            return False
        if self.phase != 'pattern':
            return False
        counts, self.counts_arm = self.counts_arm, True
        self.phase = 'body'
        return counts


class OCamlStates(CodeStateMachine):
    '''Top-level `let` bindings with arguments, or whose body starts with
    `fun` / `function`, are functions. A `let` starts a new item when it
    is outside brackets and `begin ... end`, and the token before it ends
    an expression or is `;;`; otherwise it is a local `let ... in`.
    In a function body, n `match` / `function` arms add n - 1 and each
    `try` handler arm adds one; or-pattern bars and other `->` add
    nothing.'''
    # pylint: disable=R0902

    _OPENERS = {'(', '[', '{', '[|', 'begin'}
    _CLOSERS = {')', ']', '}', '|]', 'end'}
    _NON_ENDING_KEYWORDS = {
        'and', 'as', 'assert', 'begin', 'class', 'constraint', 'do',
        'downto', 'else', 'exception', 'external', 'for', 'fun',
        'function', 'functor', 'if', 'in', 'include', 'inherit',
        'initializer', 'land', 'lazy', 'let', 'lor', 'lsl', 'lsr', 'lxor',
        'asr', 'match', 'method', 'mod', 'module', 'mutable', 'new',
        'nonrec', 'object', 'of', 'open', 'or', 'private', 'rec', 'sig',
        'struct', 'then', 'to', 'try', 'type', 'val', 'virtual', 'when',
        'while', 'with'}
    _ATOM = re.compile(r"^[\w'\"`]")

    def __init__(self, context):
        super().__init__(context)
        self._depth = 0
        self._prev_line = 0
        self._has_params = False
        self._continues_param = False
        self._after_label = False
        self._arm_scopes = []

    def __call__(self, token, reader=None):
        result = super().__call__(token, reader)
        self._prev_line = self.context.current_line
        return result

    def _is_name_or_literal(self, token):
        return self._ATOM.match(token) is not None and \
            token not in self._NON_ENDING_KEYWORDS

    def _ends_expression(self, token):
        return token in self._CLOSERS or token in ('done', '()') or \
            self._is_name_or_literal(token)

    def _state_global(self, token):
        if token in self._OPENERS:
            self._depth += 1
        elif token in self._CLOSERS:
            self._depth = max(0, self._depth - 1)
            scopes = self._arm_scopes
            while scopes and scopes[-1].depth > self._depth:
                scopes.pop()
        elif token == 'let' and self._depth == 0 and (
                self.last_token is None or self.last_token == ';;' or
                self._ends_expression(self.last_token)):
            self._start_item()
        else:
            self._track_arms(token)

    def _track_arms(self, token):
        if token in ('match', 'try'):
            self._arm_scopes.append(
                _ArmScope(self._depth, 'subject', token == 'try'))
        elif token == 'function':
            self._arm_scopes.append(_ArmScope(self._depth, 'pattern', False))
        elif token == 'with':
            self._start_arms()
        elif token in ('->', '|') and self._arm_scopes:
            scope = self._arm_scopes[-1]
            if scope.depth == self._depth and scope.arm_token(token):
                self.context.add_condition()

    def _start_arms(self):
        '''`with` starts the arms of the innermost `match` / `try` still
        in its subject at this depth, ending constructs opened since; a
        record update's `with` is inside braces and finds none.'''
        for index in range(len(self._arm_scopes) - 1, -1, -1):
            scope = self._arm_scopes[index]
            if scope.depth != self._depth:
                return
            if scope.phase == 'subject':
                del self._arm_scopes[index + 1:]
                scope.phase = 'pattern'
                return

    def _start_item(self):
        '''The `let` token was already counted in the previous item; the
        new function counts its first token and line from the start.'''
        context = self.context
        previous = context.current_function
        previous.nloc -= 1 if context.newline else 0
        previous.token_count -= 1
        if previous is not context.global_pseudo_function:
            previous.end_line = self._prev_line
            context.end_of_function()
        context.push_new_function('')
        self._arm_scopes = []
        self._has_params = False
        self._continues_param = self._after_label = False
        self._state = self._state_name

    def _state_name(self, token):
        if token == 'rec':
            return
        if self._is_name_or_literal(token):
            self.context.add_to_function_name(token)
            self._state = self._state_params
        else:
            self._drop_item()
            self.next(self._state_global, token)

    def _state_params(self, token):
        if token == '=':
            self._state = self._state_body_start
        elif token == ':' and self._after_label:
            self.context.parameter(token)
            self._continues_param, self._after_label = True, False
        elif token == ':':
            self._state = self._state_annotation
        else:
            self._add_param_token(token)
            self._after_label = self.last_token in ('~', '?') and token != '('
            self._continues_param = token in ('~', '?')
            if token == '(':
                self._depth = 1
                self._state = self._state_bracketed_param

    def _add_param_token(self, token):
        if self._has_params and not self._continues_param:
            self.context.parameter(',')
        self.context.parameter(token)
        self._has_params = True

    def _state_bracketed_param(self, token):
        self.context.parameter(token)
        if token in self._OPENERS:
            self._depth += 1
        elif token in self._CLOSERS:
            self._depth -= 1
            if self._depth == 0:
                self._continues_param = False
                self._state = self._state_params

    def _state_annotation(self, token):
        if token == '=':
            self._state = self._state_body_start

    def _state_body_start(self, token):
        if not self._has_params and token not in ('fun', 'function'):
            self._drop_item()
        self.next(self._state_global, token)

    def _drop_item(self):
        '''The binding is a value, not a function: fold it into global.'''
        context = self.context
        dropped = context.current_function
        context.current_function = context.stacked_functions.pop()
        context.current_function.nloc += dropped.nloc
        context.current_function.token_count += dropped.token_count

    def statemachine_before_return(self):
        if self.context.current_function is not \
                self.context.global_pseudo_function:
            self.context.end_of_function()
