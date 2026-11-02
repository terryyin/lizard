'''
Java method-body and class-body nesting state machines.
'''

from lizard_languages.code_reader import CodeStateMachine
from .java import JavaStates, _JAVA_BRACE_COUNT


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

        if token == 'new':
            # A field initializer may instantiate an anonymous class
            # (= new Type<...>() { ... }); handle it so the field is not parsed as
            # a method and the anonymous body's own methods are still counted.
            self.next(self._state_new)
            return

        super()._state_global(token)
        d = _JAVA_BRACE_COUNT.get(token)
        if d is not None:
            self._class_body_brace += d
        if token == '}' and self._class_body_brace == 0:
            self.statemachine_return()
