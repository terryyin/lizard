'''
Language parser for Java
'''

from lizard_languages.code_reader import CodeStateMachine
from .clike import CLikeReader, CLikeStates, CLikeNestingStackStates

# "record" is a keyword for type declarations, but a valid method/field name.
# Treat as keyword only when it cannot be the name in "Type name" (field/method/parameter).
_JAVA_CLASS_MODIFIERS = frozenset({
    'public', 'private', 'protected', 'static', 'final', 'strictfp', 'abstract',
    'synchronized', 'native', 'default', 'transient', 'volatile', 'sealed', 'non-sealed',
})
_JAVA_TYPE_KEYWORDS = frozenset({
    'void', 'boolean', 'byte', 'char', 'short', 'int', 'long', 'float', 'double', 'var',
})
_JAVA_BRACE_COUNT = {'{': 1, '}': -1}


def _java_record_begins_type_declaration(last_token, after_unqualified_annotation):
    if after_unqualified_annotation:
        return True
    if last_token is None:
        return True
    if last_token in _JAVA_TYPE_KEYWORDS:
        return False
    if last_token in (']', '>'):
        return False
    if last_token[0] in '_{$':
        return False
    if last_token[0].isalpha():
        if last_token[0].islower() and last_token in _JAVA_CLASS_MODIFIERS:
            return True
        if last_token[0].isupper():
            return False
    if last_token in ('{', '}', ';', ')', '@'):
        return True
    return False


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
        self.handling_dot_class = False
        self.handling_method_ref = False
        self._java_after_unqualified_annotation = False

    def _consume_java_expression_tokens(self, token):
        """Skip tokens that are not class declarations: Foo.class, Type::meth."""
        if token == "::":
            self.handling_method_ref = True
            return True
        if self.handling_method_ref:
            self.handling_method_ref = False
            return True
        if token == "." and not self.handling_dot_class:
            self.handling_dot_class = True
            return True
        if self.handling_dot_class:
            self.handling_dot_class = False
            if token == "class":
                return True
        return False

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
        self.sub_state(JavaFunctionBodyStates(self.context, True), callback, token)

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

    def _try_start_a_class(self, token, after_unqualified_annotation=False):
        if token in ("class", "enum"):
            self._java_after_unqualified_annotation = False
            self.class_name = None
            self.is_record = False
            self.in_record_constructor = False
            self._state = self._state_class_declaration
            return True
        if token == "record":
            if self.in_method_body:
                return False
            if not _java_record_begins_type_declaration(
                    self.last_token, after_unqualified_annotation):
                self._java_after_unqualified_annotation = False
                return False
            self._java_after_unqualified_annotation = False
            self.class_name = None
            self.is_record = True
            self.in_record_constructor = False
            self._state = self._state_class_declaration
            return True
        return False

    def _state_global(self, token):
        if self._consume_java_expression_tokens(token):
            return
        use_after_annotation = self._java_after_unqualified_annotation
        if token != "record":
            self._java_after_unqualified_annotation = False
        if token == '@':
            self._state = self._state_decorator
            return
        if self._try_start_a_class(token, use_after_annotation):
            return
        if not self.in_record_constructor:  # Only process as potential function if not in record constructor
            super(JavaStates, self)._state_global(token)

    def _state_decorator(self, _):
        self._state = self._state_post_decorator

    @CodeStateMachine.read_inside_brackets_then("()", "_state_global")
    def _state_annotation_arguments(self, token):
        """Skip (...) after @Name so inner tokens are not parsed as methods."""
        pass

    def _state_post_decorator(self, token):
        if token == '.':
            self._state = self._state_decorator
        elif token == '(':
            self.next(self._state_annotation_arguments, token)
        else:
            # @SimpleName without (...) — the name is not a method/class, e.g. @Deprecated before record/void
            self._java_after_unqualified_annotation = True
            self._state = self._state_global

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
    def __init__(self, context, exit_with_brace_depth=True):
        super(JavaFunctionBodyStates, self).__init__(context)
        self.in_method_body = True
        self.ignore_tokens = False  # Additional flag to ignore tokens that could confuse the parser
        self._exit_with_brace_depth = exit_with_brace_depth
        # Only { } (not ( )): shared br_count + ()/{} in decorators can hit 0 inside static blocks
        # while still inside the outer { }.
        self._java_block_brace = 0

    @CodeStateMachine.read_inside_brackets_then("{}", "_state_dummy")
    @CodeStateMachine.read_inside_brackets_then("()", "_state_dummy")
    def _state_global(self, token):
        if self._consume_java_expression_tokens(token):
            return
        use_after_annotation = self._java_after_unqualified_annotation
        if token != "record":
            self._java_after_unqualified_annotation = False
        if token == '@':
            self._state = self._state_decorator
            return
        if self._try_start_a_class(token, use_after_annotation):
            return
        if self.ignore_tokens:
            self.ignore_tokens = False
            return
        if token == "new":
            self.next(self._state_new)
        else:
            if self._exit_with_brace_depth:
                d = _JAVA_BRACE_COUNT.get(token)
                if d is not None:
                    self._java_block_brace += d
                    if self._java_block_brace == 0:
                        self.statemachine_return()
            elif self.br_count == 0:
                self.statemachine_return()

    def _state_dummy(self, _):
        pass

    def _state_new(self, token):
        self.next(self._state_new_parameters)

    def _state_new_parameters(self, token):
        if token == "(":
            self.sub_state(JavaFunctionBodyStates(self.context, False), None, token)
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
        self._after_static_keyword = False
        # { } that reach this state machine, plus 1 for static/instance blocks whose
        # bodies are a sub_state (} not seen at this level, balanced in callback).
        self._class_body_brace = 0

    def _state_global(self, token):
        if self._after_static_keyword:
            self._after_static_keyword = False
            if token == '{':
                self._class_body_brace += 1

                def _after_static_block():
                    self._class_body_brace -= 1
                self.sub_state(JavaFunctionBodyStates(self.context, True), _after_static_block, token)
                return
            JavaStates._state_global(self, 'static')
            JavaStates._state_global(self, token)
            if token == '}' and self._class_body_brace == 0:
                self.statemachine_return()
            return

        if token == 'static':
            self._after_static_keyword = True
            return

        # Instance initializer block: { ... } after '{', '}', or ';' at class body level
        if token == '{' and self.last_token in ('{', '}', ';'):
            self._class_body_brace += 1

            def _after_init_block():
                self._class_body_brace -= 1
            self.sub_state(JavaFunctionBodyStates(self.context, True), _after_init_block, token)
            return

        super()._state_global(token)
        d = _JAVA_BRACE_COUNT.get(token)
        if d is not None:
            self._class_body_brace += d
        if token == '}' and self._class_body_brace == 0:
            self.statemachine_return()
