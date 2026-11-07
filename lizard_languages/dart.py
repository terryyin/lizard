'''
Language parser for Dart
'''

from .code_reader import CodeReader
from .clike import CLikeReader, CLikeStates, CLikeNestingStackStates


class DartReader(CLikeReader):
    # pylint: disable=R0903

    ext = ['dart']
    language_names = ['dart']

    _control_flow_keywords = CodeReader._control_flow_keywords | {'on'}
    _logical_operators = {'&&', '||', '??', '??='}

    def __init__(self, context):
        super(DartReader, self).__init__(context)
        self.parallel_states = [
            DartStates(context),
            DartNestingStackStates(context)
        ]
        self._prev_token = None
        self._suppress_next_catch = False
        self._switch_subject_paren = None
        self._switch_stack = []

    def process_token(self, token):
        # `on T catch` is one clause: undo the automatic catch decision.
        if token == 'on':
            self._suppress_next_catch = True
        elif token == 'catch' and self._suppress_next_catch:
            self.context.add_condition(-1)
            self._suppress_next_catch = False
        elif token == '{' and self._suppress_next_catch:
            self._suppress_next_catch = False

        self._track_switch_expression(token)

        if not token.isspace():
            self._prev_token = token
        return False

    def _track_switch_expression(self, token):
        # After `switch`, wait for the subject's braces to open.
        if self._switch_subject_paren is not None:
            if token == '(':
                self._switch_subject_paren += 1
            elif token == ')':
                if self._switch_subject_paren > 0:
                    self._switch_subject_paren -= 1
            elif token == '{' and self._switch_subject_paren == 0:
                self._switch_stack.append({'brace': 1, 'paren': 0})
                self._switch_subject_paren = None
            return

        if token == 'switch' and self._prev_token != '.':
            self._switch_subject_paren = 0
            return

        if not self._switch_stack:
            return

        sw = self._switch_stack[-1]
        if token == '{':
            sw['brace'] += 1
        elif token == '}':
            sw['brace'] -= 1
            if sw['brace'] == 0:
                self._switch_stack.pop()
        elif token in '([':
            sw['paren'] += 1
        elif token in ')]':
            if sw['paren'] > 0:
                sw['paren'] -= 1
        elif (token == '=>' and sw['brace'] == 1 and sw['paren'] == 0
              and self._prev_token != '_'):
            self.context.add_condition()

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        # Longest first, subgroup-free: triple/raw strings ahead of base
        # quotes; then nullable names, ??=, ??, ?.., ?.
        # `(?!\.)` keeps int?/T? from swallowing the ? of ?. and ?..
        # Closing ?> generics stay the existing <…?> token.
        return CLikeReader.generate_tokens(
            source_code,
            r"|(?:r)?'''[\s\S]*?'''" +
            r'|(?:r)?"""[\s\S]*?"""' +
            r"|r'(?:\\.|[^'\\])*?'" +
            r'|r"(?:\\.|[^"\\])*"' +
            r"|\w+\?(?!\.)" +
            r"|\?\?=" +
            r"|\?\?" +
            r"|\?\.\." +
            r"|\?\." +
            addition,
            token_class
        )


class DartStates(CLikeStates):  # pylint: disable=R0903
    """Dart declarations: => bodies, accessors, constructors, async/generators."""

    def __init__(self, context):
        super(DartStates, self).__init__(context)
        self._expr_nesting = 0
        self._init_list_depth = 0
        self._accessor_kind = None

    def _state_global(self, token):
        # factory/const are modifiers; they do not name a function.
        if token in ('factory', 'const'):
            return
        if token in ('get', 'set'):
            self._accessor_kind = token
            self._state = self._state_accessor_name
            return
        super(DartStates, self)._state_global(token)

    def _state_accessor_name(self, token):
        # get/set as ordinary identifiers (map.get, set.add) fall through.
        if token in ('.', '('):
            self._resume_get_or_set_as_name(token)
            return
        if token[0].isalpha() or token[0] == '_':
            is_setter = self._accessor_kind == 'set'
            self._accessor_kind = None
            name = token + ('=' if is_setter else '')
            self.context.try_new_function(name)
            if is_setter:
                self._state = self._state_function
            else:
                # Getters have no parameter list; body follows immediately.
                self._state = self._state_dec_to_imp
            return
        self._state = self._state_global
        self._resume_get_or_set_as_name(token)

    def _resume_get_or_set_as_name(self, token):
        kind = self._accessor_kind
        self._accessor_kind = None
        self.try_new_function(kind)
        self._state_function(token)

    def _current_type_name(self):
        prefix = self.context.with_namespace('')
        if prefix.endswith('::'):
            return prefix[:-2].split('::')[-1]
        return None

    def _state_function(self, token):
        # Point.named( — join when '.' follows a name matching the class.
        if token == '.' and self.context.current_function:
            type_name = self._current_type_name()
            if (type_name and
                    self.context.current_function.unqualified_name == type_name):
                self._state = self._state_named_constructor
                return
        super(DartStates, self)._state_function(token)

    def _state_named_constructor(self, token):
        if token[0].isalpha() or token[0] == '_':
            self.try_new_function(token)
            return
        self.next(self._state_global, token)

    def _enter_expression_body(self):
        self.context.confirm_new_function()
        # Claim function nesting now so a later switch/map `{` is bare,
        # not treated as the function's brace boundary.
        self.context.add_bare_nesting()
        self._expr_nesting = 0
        self._state = self._expression_body

    def _end_expression_body(self):
        self.context.pop_nesting()
        self._state = self._state_global

    def _state_dec_to_imp(self, token):
        # async / sync / async* / sync* sit between ) and { or =>.
        if token in ('async', 'sync', '*'):
            return
        if token == '=>':
            self._enter_expression_body()
            return
        if token == ':':
            self._init_list_depth = 0
            self._state = self._state_initialization_list
            return
        super(DartStates, self)._state_dec_to_imp(token)

    def _state_initialization_list(self, token):
        # Replace C++ init-list: at depth zero until '{', '=>', or ';'.
        if self._init_list_depth == 0:
            if token == '{':
                self.next(self._state_entering_imp, "{")
                return
            if token == '=>':
                self._enter_expression_body()
                return
            if token == ';':
                # Body-less declaration — not a function.
                self._state = self._state_global
                return
        if token in '{([':
            self._init_list_depth += 1
            return
        if token in ')]}':
            if self._init_list_depth > 0:
                self._init_list_depth -= 1

    def _expression_body(self, token):
        # Depth-tracked so nested (e) => … and containers do not end early.
        if token in '{([':
            self._expr_nesting += 1
            return
        if token in ')]}':
            if self._expr_nesting > 0:
                self._expr_nesting -= 1
            return
        if self._expr_nesting == 0 and token == ';':
            self._end_expression_body()

    def statemachine_before_return(self):
        if self._state == self._expression_body:
            self._end_expression_body()


class DartNestingStackStates(CLikeNestingStackStates):  # pylint: disable=R0903
    """Nesting for Dart types, including mixin and enum (not extension)."""

    _TYPE_KEYWORDS = CLikeNestingStackStates._TYPE_KEYWORDS + ("mixin", "enum")
