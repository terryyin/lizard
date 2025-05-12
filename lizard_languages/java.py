'''
Language parser for Java
'''

from lizard_languages.code_reader import CodeStateMachine
from .clike import CLikeReader, CLikeStates, CLikeNestingStackStates


class JavaReader(CLikeReader):
    # pylint: disable=R0903

    ext = ['java']
    language_names = ['java']

    def __init__(self, context):
        super(JavaReader, self).__init__(context)
        self.parallel_states = [
                JavaStates(context),
                CLikeNestingStackStates(context)]


class JavaStates(CLikeStates):  # pylint: disable=R0903
    def __init__(self, context):
        super(JavaStates, self).__init__(context)
        self.class_name = None
        self.is_record = False
        self.in_record_constructor = False
        self.in_method_body = False

    def _state_old_c_params(self, token):
        if token == '{':
            self._state_dec_to_imp(token)

    def _state_imp(self, token):
        # When entering a function implementation, set the flag
        self.in_method_body = True

        def callback():
            # When exiting the function implementation, clear the flag
            self.in_method_body = False
            self.next(self._state_global)
        self.sub_state(JavaFunctionBodyStates(self.context), callback, token)

    def try_new_function(self, name):
        # Don't create a function for record compact constructor
        if self.is_record and name == self.class_name:
            self.in_record_constructor = True
            self._state = self._state_record_compact_constructor
            return
        self.context.try_new_function(name)
        self._state = self._state_function
        if self.class_name and self.context.current_function:
            self.context.current_function.name = f"{self.class_name}::{name}"

    def _try_start_a_class(self, token):
        if token in ("class", "record", "enum"):
            self.class_name = None
            self.is_record = token == "record"
            self.in_record_constructor = False
            self._state = self._state_class_declaration
            return True

    def _state_global(self, token):
        if token == '@':
            self._state = self._state_decorator
            return
        if self._try_start_a_class(token):
            return
        if not self.in_record_constructor:  # Only process as potential function if not in record constructor
            super(JavaStates, self)._state_global(token)

    def _state_decorator(self, _):
        self._state = self._state_post_decorator

    def _state_post_decorator(self, token):
        if token == '.':
            self._state = self._state_decorator
        else:
            self._state = self._state_global
            self._state(token)

    def _state_class_declaration(self, token):
        if token == '{':
            def callback():
                self._state = self._state_global
            self.sub_state(JavaClassBodyStates(self.class_name, self.is_record, self.context), callback, token)
        elif token == '(':  # Record parameters
            self._state = self._state_record_parameters
        elif token[0].isalpha():
            if not self.class_name:  # Only set class name if not already set
                self.class_name = token

    def _state_record_parameters(self, token):
        if token == ')':
            self._state = self._state_class_declaration

    def _state_record_compact_constructor(self, token):
        if token == '{':
            self._state = self._state_record_constructor_body
            return
        self._state = self._state_global
        self._state(token)

    def _state_record_constructor_body(self, token):
        if token == '}':
            self.in_record_constructor = False
            self._state = self._state_global


class JavaFunctionBodyStates(JavaStates):
    def __init__(self, context):
        super(JavaFunctionBodyStates, self).__init__(context)
        self.in_method_body = True
        self.ignore_tokens = False  # Additional flag to ignore tokens that could confuse the parser
        self.handling_dot_class = False  # Flag to handle .class token specifically
        self.handling_method_ref = False  # Flag to handle method references

    @CodeStateMachine.read_inside_brackets_then("{}", "_state_dummy")
    @CodeStateMachine.read_inside_brackets_then("()", "_state_dummy")
    def _state_global(self, token):
        # Handle method references (::new, ::methodName)
        if token == "::":
            self.handling_method_ref = True
            return
        if self.handling_method_ref:
            self.handling_method_ref = False
            return

        # Special handling for .class token
        if token == "." and not self.handling_dot_class:
            self.handling_dot_class = True
            return
        if self.handling_dot_class:
            self.handling_dot_class = False
            if token == "class":
                return  # Skip the 'class' token after a dot

        # Special handling for tokens that could confuse the parser
        if self.ignore_tokens:
            self.ignore_tokens = False
            return

        if token == "new":
            self.next(self._state_new)
        else:
            # Always try to parse class declarations, even in method bodies
            # This ensures that local classes are properly detected
            if self._try_start_a_class(token):
                return

            if self.br_count == 0:
                self.statemachine_return()

    def _state_dummy(self, _):
        pass

    def _state_new(self, token):
        self.next(self._state_new_parameters)

    def _state_new_parameters(self, token):
        if token == "(":
            self.sub_state(JavaFunctionBodyStates(self.context), None, token)
            return
        if token == "{":
            def callback():
                self.next(self._state_global)
            self.sub_state(JavaClassBodyStates("(anonymous)", False, self.context), callback, token)
            return
        self.next(self._state_global, token)


class JavaClassBodyStates(JavaStates):
    def __init__(self, class_name, is_record, context):
        super(JavaClassBodyStates, self).__init__(context)
        self.class_name = class_name
        self.is_record = is_record

    def _state_global(self, token):
        super()._state_global(token)
        if token == '}':
            self.statemachine_return()
