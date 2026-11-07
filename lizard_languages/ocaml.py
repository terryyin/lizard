'''
Language parser for OCaml
'''

import re
from .code_reader import CodeReader, CodeStateMachine
from .pygments_fragments import join_fragments, lex_keeping_newlines
from .match_arms import ArmTracker


def _joins_bars(run, value):
    '''OCaml's lexer splits `||` into two `|`.'''
    return value == '|' and run in ('', '|')


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
        return join_fragments(lex_keeping_newlines(source_code, 'ocaml'),
                              joins=_joins_bars)


class OCamlStates(CodeStateMachine):
    '''Item `let` / `and` bindings with arguments, or whose body starts
    with `fun` / `function`, are functions. A heading token starts a new
    item when it is at item depth (outside brackets and `begin ... end`,
    directly inside a module's `struct`), and the token before it ends an
    expression, is `;;`, or opens the struct; otherwise it is local.
    `type`, `module`, `open`, ... items and `;;` end the function, as does
    its struct's `end`. In a function body, n `match` / `function` arms
    add n - 1 and each `try` handler arm adds one; or-pattern bars and
    other `->` add nothing.'''
    # pylint: disable=R0902

    _OPENERS = {'(', '[', '{', '[|', 'begin', 'struct', 'sig', 'object'}
    _CLOSERS = {')', ']', '}', '|]', 'end'}
    _OTHER_ITEMS = {'type', 'module', 'open', 'include', 'exception',
                    'external'}
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
        self._arms = ArmTracker()
        self._param_depth = 0
        self._structs = []      # item depth inside each module struct
        self._local_lets = []   # depth of each local `let` awaiting `in`
        self._item_kind = None

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

    def _item_depth(self):
        return self._structs[-1] if self._structs else 0

    def _in_heading_position(self):
        return self._depth == self._item_depth() and (
            self.last_token in (None, ';;', 'struct') or
            self._ends_expression(self.last_token))

    def _state_global(self, token):
        if token in self._OPENERS:
            self._open(token)
        elif token in self._CLOSERS:
            self._close()
        elif token == ';;' and self._depth == self._item_depth():
            self._start_other_item(token)
        elif token in self._OTHER_ITEMS and self._in_heading_position():
            self._start_other_item(token)
        elif token == 'let' and self._in_heading_position():
            self._start_item(token)
        elif token == 'let':
            self._local_lets.append(self._depth)
        elif token == 'in' and self._local_lets:
            self._local_lets.pop()
        elif token == 'and' and self._item_kind == 'let' and \
                self._depth == self._item_depth() and \
                self._depth not in self._local_lets:
            self._start_item(token)
        elif self._arms.adds_decision(token, self._depth):
            self.context.add_condition()

    def _open(self, token):
        '''A module item's `struct` holds items of its own.'''
        if token == 'struct' and self._item_kind == 'module' and \
                self._depth == self._item_depth():
            self._structs.append(self._depth + 1)
            self._item_kind = None
        self._depth += 1

    def _close(self):
        '''A closer ends constructs opened inside it; a module struct's
        `end` ends its last function at that line.'''
        self._depth = max(0, self._depth - 1)
        self._arms.close_to(self._depth)
        while self._local_lets and self._local_lets[-1] > self._depth:
            self._local_lets.pop()
        if self._structs and self._structs[-1] > self._depth:
            self._structs.pop()
            self._item_kind = 'module'
            self._arms = ArmTracker()
            self._end_function()

    def _end_function(self, end_line=None):
        '''End the open function, if any; global stays open.'''
        function = self.context.current_function
        if function is not self.context.global_pseudo_function:
            function.end_line = end_line or function.end_line
            self.context.end_of_function()

    def _end_item(self, token):
        '''The heading token was already counted in the previous item;
        return its line share after taking it back.'''
        context = self.context
        previous = context.current_function
        line = 1 if context.newline else 0
        previous.nloc -= line
        previous.token_count -= 1
        self._end_function(self._prev_line)
        self._arms, self._local_lets = ArmTracker(), []
        self._item_kind = 'let' if token == 'and' else token
        return line

    def _start_other_item(self, token):
        '''A non-`let` item belongs to no function: global keeps it.'''
        line = self._end_item(token)
        self.context.current_function.nloc += line
        self.context.current_function.token_count += 1

    def _start_item(self, token):
        '''The new function counts its first token and line from the
        start.'''
        self._end_item(token)
        self.context.push_new_function('')
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
            self.fold_into_enclosing_function()
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
                self._param_depth = 1
                self._state = self._state_bracketed_param

    def _add_param_token(self, token):
        if self._has_params and not self._continues_param:
            self.context.parameter(',')
        self.context.parameter(token)
        self._has_params = True

    def _state_bracketed_param(self, token):
        self.context.parameter(token)
        if token in self._OPENERS:
            self._param_depth += 1
        elif token in self._CLOSERS:
            self._param_depth -= 1
            if self._param_depth == 0:
                self._continues_param = False
                self._state = self._state_params

    def _state_annotation(self, token):
        if token == '=':
            self._state = self._state_body_start

    def _state_body_start(self, token):
        if not self._has_params and token not in ('fun', 'function'):
            self.fold_into_enclosing_function()
        self.next(self._state_global, token)

    def statemachine_before_return(self):
        self._end_function()
