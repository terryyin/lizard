''' Language parser for Python '''

from .code_reader import CodeStateMachine
from .clike import CLikeReader, CLikeStates, CLikeNestingStackStates


class ObjCReader(CLikeReader):

    ext = ['m', 'mm']
    language_names = ['objectivec', 'objective-c', 'objc']

    def __init__(self, context):
        super(ObjCReader, self).__init__(context)
        self.parallel_states = [
                ObjCStates(context),
                CLikeNestingStackStates(context)]

    def fake_and_useless(self):
        pass

    def useless_and_fake(self):
        pass


class ObjCStates(CLikeStates):  # pylint: disable=R0903
    # @interface and @protocol declare an API; a body may only appear in
    # @implementation. Their contents must not reach the C-like states, where
    # "Name () { ivars }" of a class extension looks like a function definition
    # and is reported as one (issue #305).
    def __init__(self, context):
        super(ObjCStates, self).__init__(context)
        self._objc_param_paren_depth = 0
        self._objc_after_at = False

    def _state_global(self, token):
        if self._objc_after_at:
            self._objc_after_at = False
            if token == 'interface':
                self._state = self._state_objc_declaration
                return
            if token == 'protocol':
                self._state = self._state_objc_protocol_header
                return
        if token == '@':
            self._objc_after_at = True
        super(ObjCStates, self)._state_global(token)
        if token == 'typedef':
            self.next(self._typedef, token)
        elif token == '(':
            self.next(self._state_dec, token)

    def _state_objc_declaration(self, token):
        """Skip an @interface / @protocol section up to its @end."""
        if self._objc_after_at:
            self._objc_after_at = False
            if token == 'end':
                self._state = self._state_global
                return
        if token == '@':
            self._objc_after_at = True

    def _state_objc_protocol_header(self, token):
        """Distinguish definitions from forward declarations and expressions."""
        if token == '(':
            self._state = self._state_objc_protocol_expression
        elif token == ';':
            self._state = self._state_global
        elif token == '\n':
            self._state = self._state_objc_declaration

    def _state_objc_protocol_expression(self, token):
        """Skip the argument of a file-scope @protocol(Name) expression."""
        if token == ')':
            self._state = self._state_global

    def _state_dec_to_imp(self, token):
        if token in ("+", "-"):
            self._state = self._state_global
        else:
            super(ObjCStates, self)._state_dec_to_imp(token)
            if self._state != self._state_imp:
                self._state = self._state_objc_dec_begin
                self.context.restart_new_function(token)

    def _state_objc_dec_begin(self, token):
        if token == ':':
            self._state = self._state_objc_dec
            self.context.add_to_function_name(token)
        elif token == '{':
            self.next(self._state_entering_imp, "{")
        else:
            self._state = self._state_global

    def _state_objc_dec(self, token):
        if token == '(':
            self._objc_param_paren_depth = 0
            self._state = self._state_objc_param_type
            self.context.add_to_long_function_name(token)
        elif token == ',':
            pass
        elif token == '{':
            self.next(self._state_entering_imp, "{")
        else:
            self._state = self._state_objc_dec_begin
            self.context.add_to_function_name(" " + token)

    def _state_objc_param_type(self, token):
        # A block / function-pointer param type, e.g. (void (^)(int)), nests
        # parentheses; balance them so the type ends at its matching ')'
        # rather than the first inner one (issue #365).
        if token == '(':
            self._objc_param_paren_depth += 1
        elif token == ')':
            if self._objc_param_paren_depth > 0:
                self._objc_param_paren_depth -= 1
            else:
                self._state = self._state_objc_param
        self.context.add_to_long_function_name(" " + token)

    def _state_objc_param(self, _):
        self._state = self._state_objc_dec

    @CodeStateMachine.read_until_then(';')
    def _typedef(self, _, tokens):
        self.next(self._state_global)
