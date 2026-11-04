'''
Language parser for F#
'''

import re
from .code_reader import CodeReader, CodeStateMachine
from .fsharp_tokens import generate_tokens as generate_fsharp_tokens, \
    is_operator
from .fsharp_items import CONTAINER_HEADINGS, OffsideItems
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
    operator. A deeper line continues the item, except under a `type`,
    `module` or `namespace` heading, or an `and` continuing a `type`
    group, where it starts a nested item. A
    `let` / `and` item is a function when it takes arguments or its body
    starts with `fun` / `function`; a `member` / `override` / `default`
    item when it takes arguments. Other items belong to no function. A
    heading right after an item's attribute group heads that item.
    In an item, n `match` / `function` arms add n - 1, each `try`
    handler arm adds one, and a `when` in an arm's pattern adds one;
    or-pattern bars and other `->` and `when` add nothing.'''
    # pylint: disable=R0902

    _OPENERS = {'(', '[', '{', '[|', '[<'}
    _CLOSERS = {')', ']', '}', '|]', '>]'}
    _MODIFIERS = {'rec', 'inline', 'private', 'internal', 'public',
                  'mutable'}
    _NAME = re.compile(r"^[\w`]")
    _LETS = {'let', 'and'}
    _MEMBERS = {'member', 'override', 'default', 'static'}
    _BINDINGS = _LETS | _MEMBERS
    _HEADINGS = _BINDINGS | CONTAINER_HEADINGS

    def __init__(self, context):
        super().__init__(context)
        self._depth = 0
        self._items = OffsideItems()
        self._self_name, self._lambda_heads = None, False
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
        if self._attribute_item and self.last_token == '>]' and \
                token in self._HEADINGS:
            self._items.reheaded(token)
            return True
        column = getattr(token, 'column', None)
        return column is not None and not is_operator(token) and \
            self._items.starts_item(column, token)

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
        self._attribute_item = token == '[<'
        self._arms = ArmTracker()
        if token in self._BINDINGS and not self._items.in_container():
            context.push_new_function('')
            self._candidate, self._has_params = True, False
            self._lambda_heads = token in self._LETS
            self._state = self._state_name if token in self._LETS \
                else self._state_member
        else:
            context.current_function.nloc += 1
            context.current_function.token_count += 1
            self._state = self._state_global

    def _state_member(self, token):
        '''A member name may follow a self identifier and `.`; `static
        let` is a let, and `member val` an auto-property.'''
        if token == 'let':
            self._state, self._lambda_heads = self._state_name, True
        elif token == 'member' or token in self._MODIFIERS:
            return
        elif token != 'val' and self._NAME.match(token):
            self._self_name = token
            self._state = self._state_member_dot
        else:
            self._drop_item()

    def _state_member_dot(self, token):
        if token == '.':
            self._state = self._state_name
        else:
            self.context.add_to_function_name(self._self_name)
            self._state = self._state_params
            self._state_params(token)

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
        if not self._has_params and not (
                self._lambda_heads and token in ('fun', 'function')):
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
