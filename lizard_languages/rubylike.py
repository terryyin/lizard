'''
Language parser for Ruby-like languages
'''

from .code_reader import CodeReader, CodeStateMachine
from .script_language import ScriptLanguageMixIn


def state_embedded_doc(token):
    if token == "=end":
        return True


# Ruby expects an expression after these words, so an `if` that follows
# one starts an expression: `else if c ... end`.
_EXPRESSION_KEYWORDS = frozenset((
    'and', 'or', 'not', 'then', 'else', 'elsif', 'do', 'begin', 'ensure',
    'case', 'when', 'in', 'if', 'unless', 'while', 'until', 'for'))


class RubylikeStateMachine(CodeStateMachine):
    # pylint: disable=R0903
    FUNC_KEYWORD = 'def'
    # Ruby needs the parameter list on the line of `def`.
    PARAMETERS_ON_DEF_LINE = True
    # Each of these opens a block that ends at its own `end` or `}`.
    BLOCK_KEYWORDS = ("begin", "do", "class", "module", "case", "{", "${")
    LOOP_KEYWORDS = ("while", "until", "for")

    def __init__(self, context):
        super(RubylikeStateMachine, self).__init__(context)
        self.loop_br_count = 0

    def _state_global(self, token):
        # A keyword that the reader marks as a method name, `m.end(0)`,
        # changes no state.
        if token in ("end", "}"):
            if not getattr(token, 'is_member_name', False):
                self.statemachine_return()
        elif token == self.FUNC_KEYWORD:
            if not getattr(token, 'is_member_name', False):
                self._state = self._def
                self.next(self._def)
        elif token == 'it':
            if not getattr(token, 'is_member_name', False):
                self._state = self._it
        elif token in self.BLOCK_KEYWORDS and self.last_token != ".":
            self.sub_state(self.statemachine_clone())
        elif token in self.LOOP_KEYWORDS:
            if not getattr(token, 'is_member_name', False) and \
                    not self.follows_value():
                self.next(self._for_while)
        elif token in ("if", "unless"):
            if getattr(token, 'is_member_name', False):
                pass
            elif self.follows_value():
                self.next(self._if)
            else:
                self.sub_state(self.statemachine_clone())
        elif token == "=begin":
            self.sub_state(state_embedded_doc)

    def is_newline(self):
        return self.context.newline or self.last_token == ";"

    def follows_value(self):
        """After a value, `x if c` and `x while c` are modifiers that open
        no block.  At the start of a statement, or after an operator or a
        word such as `then`, they start a block: `x = if c`."""
        last = self.last_token
        if last is None or self.is_newline():
            return False
        if getattr(last, 'is_member_name', False):
            return True
        if last in _EXPRESSION_KEYWORDS:
            return False
        end = last[-1]
        return end.isalnum() or end in '_)]}"\'`' or (
            len(last) > 1 and end in '?!/') or (
            last[0] == '%' and len(last) > 2)

    def _def(self, token):
        if token == '(':
            self.context.push_new_function('(anonymous)')
            self.next(self._def_parameters)
            return
        self.context.push_new_function(token)
        self.next(self._def_continue)

    def _it(self, token):
        if token in ('do', '{'):
            self.context.push_new_function(self.last_token)
            self.next(self._def_continue)

    def _def_continue(self, token):
        def callback():
            self.context.end_of_function()
            self.next(self._state_global)
        if token == ".":
            self.context.add_to_function_name(token)
            self.next(self._def_class_method)
        elif token == "(" and not (
                self.PARAMETERS_ON_DEF_LINE and self.is_newline()):
            # On the next line, `(a || b) if c` starts the body.
            self.next(self._def_parameters)
        else:
            self.sub_state(self.statemachine_clone(), callback, token)

    def _def_class_method(self, token):
        self.context.add_to_function_name(token)
        self.next(self._def_continue)

    def _def_parameters(self, token):
        if token == ')':
            self.next(self._def_continue)
        else:
            self.context.parameter(token)
            return
        self.context.add_to_long_function_name(" " + token)

    def _if(self, token):
        # A modifier `if` waits for a `then` (Lua) until the line ends.  A
        # `}` that it did not open ends a block: `{ |x| return x if x }`.
        if self.is_newline() or token == "}" and not self.br_count:
            self.br_count = 0
            self.next(self._state_global, token)
        elif token == "then" and not getattr(token, 'is_member_name', False):
            self.next(self._state_global)
            self.sub_state(self.statemachine_clone())
        elif token in ("{", "${"):
            self.br_count += 1
        elif token == "}":
            self.br_count -= 1

    def _for_while(self, token):
        self._loop_head(token, self._state_global, None)

    def _loop_head(self, token, state, callback):
        """The loop body starts after `do`, or with the first token of the
        next line.  A bracket or a block in the condition goes on over
        lines: `while jobs.any? { |job|`."""
        if not self.loop_br_count:
            if token == "do" and \
                    not getattr(token, 'is_member_name', False):
                self.next(state)
                self.sub_state(self.statemachine_clone(), callback)
                return
            if self.is_newline():
                self.next(state)
                self.sub_state(self.statemachine_clone(), callback, token)
                return
        if token in ("(", "[", "${") or (
                token == "{" and self.last_token != ")"):
            self.loop_br_count += 1
        elif token in (")", "]", "}") and self.loop_br_count:
            self.loop_br_count -= 1


class RubylikeReader(CodeReader, ScriptLanguageMixIn):
    # pylint: disable=R0903

    # Separated condition categories
    _control_flow_keywords = {'if', 'elsif', 'elseif', 'until', 'for', 'while',
                              'rescue', 'ensure', 'when'}
    _logical_operators = {'and', 'or', '||', '&&'}  # Both word and symbol forms
    _case_keywords = set()  # Ruby uses 'when' for case expressions
    _ternary_operators = {'?'}

    def __init__(self, context):
        super(RubylikeReader, self).__init__(context)
        self.parallel_states = [RubylikeStateMachine(context)]
