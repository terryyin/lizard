'''
Language parser for F#
'''

import re
from .code_reader import CodeReader, CodeStateMachine
from .fsharp_tokens import generate_tokens as generate_fsharp_tokens, \
    is_operator
from .match_arms import ArmTracker


class FSharpReader(CodeReader):
    # pylint: disable=R0903

    ext = ['fs', 'fsx']
    language_names = ['fsharp']

    _control_flow_keywords = {'if', 'elif', 'for', 'while'}
    _logical_operators = {'&&', '||'}
    _case_keywords = set()
    _ternary_operators = set()

    def __init__(self, context):
        super().__init__(context)
        self.parallel_states = [FSharpStates(context)]

    @staticmethod
    def get_comment_from_token(token):
        if token.startswith('(*'):
            return token[2:-2]
        if token.startswith('//'):
            return token[2:]
        return None

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        return generate_fsharp_tokens(
            source_code,
            lambda token: FSharpReader.get_comment_from_token(token)
            is not None)


class FSharpStates(CodeStateMachine):
    '''Light syntax items follow the offside rule: a line whose first code
    token, with brackets closed, is at or left of the current item's
    column starts a new item, unless it begins with `|` or another infix
    operator. A `let` item is a function when it takes arguments or its
    body starts with `fun` / `function`; any other item belongs to no
    function. Everything deeper belongs to the item. A `let` right after
    the attribute group that starts an item heads the item instead.
    In an item, n `match` / `function` arms add n - 1, each `try`
    handler arm adds one, and a `when` in an arm's pattern adds one;
    or-pattern bars and other `->` and `when` add nothing.'''
    # pylint: disable=R0902

    _OPENERS = {'(', '[', '{', '[|', '[<'}
    _CLOSERS = {')', ']', '}', '|]', '>]'}
    _MODIFIERS = {'rec', 'inline', 'private', 'internal', 'public',
                  'mutable'}
    _NAME = re.compile(r"^[\w`]")

    def __init__(self, context):
        super().__init__(context)
        self._depth = 0
        self._item_column = None
        self._prev_line = 0
        self._candidate = False
        self._has_params = False
        self._angles = 0
        self._attribute_item = False
        self._arms = ArmTracker()

    def __call__(self, token, reader=None):
        starts_item = self._starts_item(token)
        if token in self._OPENERS:
            self._depth += 1
        elif token in self._CLOSERS:
            self._depth = max(0, self._depth - 1)
            self._arms.close_to(self._depth)
        if starts_item:
            self._start_item(token)
            result = None
        else:
            result = super().__call__(token, reader)
        if self._adds_decision(token):
            self.context.add_condition()
        self._prev_line = self.context.current_line
        return result

    def _adds_decision(self, token):
        if token == 'when':
            return self._arms.in_pattern(self._depth)
        return self._arms.adds_decision(token, self._depth)

    def _starts_item(self, token):
        if self._depth != 0:
            return False
        if token == 'let' and self._attribute_item and \
                self.last_token == '>]':
            return True
        column = getattr(token, 'column', None)
        return column is not None and not is_operator(token) and \
            (self._item_column is None or column <= self._item_column)

    def _start_item(self, token):
        '''The heading's line and token, counted in the previous item,
        move to the new one.'''
        if self._candidate:
            self._drop_item()
        context = self.context
        previous = context.current_function
        previous.nloc -= 1
        previous.token_count -= 1
        if previous is not context.global_pseudo_function:
            previous.end_line = self._prev_line
            context.end_of_function()
        self._item_column = getattr(token, 'column', self._item_column)
        self._attribute_item = token == '[<'
        self._arms = ArmTracker()
        if token == 'let':
            context.push_new_function('')
            self._candidate, self._has_params = True, False
            self._state = self._state_name
        else:
            context.current_function.nloc += 1
            context.current_function.token_count += 1
            self._state = self._state_global

    def _state_name(self, token):
        if token in self._MODIFIERS:
            return
        if self._NAME.match(token):
            self.context.add_to_function_name(token)
            self._state = self._state_params
        else:
            self._drop_item()

    def _state_params(self, token):
        if token == '<' and not self._has_params:
            self._angles = 1
            self._state = self._state_generics
        elif token == '=':
            self._state = self._state_body_start
        elif token == ':':
            self._state = self._state_annotation
        elif token == ',':
            self._drop_item()
        elif token == '()':
            self.context.add_to_long_function_name(' ()')
            self._has_params = True
        elif token == '(':
            self._open_param()
            self.context.add_to_long_function_name(' (')
            self._state = self._state_bracketed_param
        else:
            self._open_param()
            self.context.parameter(token)

    def _open_param(self):
        '''The next parameter token starts a new parameter.'''
        self._has_params = True
        if self.context.current_function.full_parameters:
            self.context.parameter(',')

    def _state_generics(self, token):
        self._angles += self._angle_change(token)
        if self._angles <= 0:
            self._angles = 0
            self._state = self._state_body_start \
                if token.endswith('=') else self._state_params

    def _state_bracketed_param(self, token):
        '''A comma directly inside the parameter's brackets separates
        tuple elements; a deeper one belongs to its element.'''
        self._angles += self._angle_change(token)
        if self._depth == 0:
            self.context.add_to_long_function_name(' )')
            self._angles = 0
            self._state = self._state_params
        elif token == ',' and self._depth == 1 and self._angles <= 0:
            self._open_param()
        elif token == ',':
            self.context.add_to_long_function_name(' ,')
        else:
            self.context.parameter(token)

    @staticmethod
    def _angle_change(token):
        '''`<` opens a generic argument list; a `>` run closes some.'''
        if token == '<':
            return 1
        if is_operator(token) and token.startswith('>'):
            return len(token.lstrip('>')) - len(token)
        return 0

    def _state_annotation(self, token):
        if token == '=' and self._depth == 0:
            self._state = self._state_body_start

    def _state_body_start(self, token):
        self._candidate = False
        if not self._has_params and token not in ('fun', 'function'):
            self._drop_item()
        self._state = self._state_global

    def _drop_item(self):
        '''The binding is a value, not a function.'''
        self.fold_into_enclosing_function()
        self._candidate = False
        self._state = self._state_global

    def statemachine_before_return(self):
        if self._candidate:
            self._drop_item()
        function = self.context.current_function
        if function is not self.context.global_pseudo_function:
            self.context.end_of_function()
