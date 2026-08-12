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
# Reserved words that cannot be method names (JLS 3.9); skip so statements are not
# reported as methods. "synchronized" is also a modifier — that form still works
# because a later type/name token starts the method.
_JAVA_STATEMENT_KEYWORDS = frozenset({
    'if', 'else', 'for', 'while', 'do', 'switch', 'catch', 'try', 'finally',
    'synchronized', 'return', 'throw', 'assert', 'break', 'continue', 'instanceof',
})


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
        self._new_generic_depth = 0

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
        if token in _JAVA_STATEMENT_KEYWORDS:
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

    def _state_new(self, token):
        self._new_generic_depth = 0
        self.next(self._state_new_parameters)

    def _state_new_parameters(self, token):
        if self._new_generic_depth > 0 or token.startswith("<"):
            # Skip the instantiated type's generic arguments, e.g. the <Long> in
            # new ThreadLocal<Long>() or the <? super T> wildcard token, so they
            # don't end the search for '(' / '{'.
            self._new_generic_depth += token.count("<") - token.count(">")
            return
        if token == "(":
            self.sub_state(JavaFunctionBodyStates(self.context, False), None, token)
            return
        if token == "{":
            def callback():
                self.next(self._state_global)
            self.sub_state(JavaClassBodyStates("(anonymous)", False, self.context), callback, token)
            return
        if token == "." or token[0].isalpha() or token[0] == '_':
            # Unqualified or qualified type name segment before '(' / '{'.
            return
        self.next(self._state_global, token)


# Nested body states subclass JavaStates; import after the class is defined.
from .java_body_states import JavaClassBodyStates, JavaFunctionBodyStates  # noqa: E402  pylint: disable=wrong-import-position
