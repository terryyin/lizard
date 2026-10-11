"""JavaScript binding patterns and ownership of initializer complexity."""

from collections import namedtuple


BindingEvent = namedtuple('BindingEvent', [
    'ended', 'in_expression', 'initializer_started',
    'initializer_ended', 'root_separator',
])
BindingSummary = namedtuple('BindingSummary', [
    'defaults', 'expression_complexity', 'confirmed_defaults',
])


class BindingFrame:
    def __init__(self, closing):
        self.closing = closing
        self.initializing = False


class JavaScriptBindings:
    """Recognize defaults in binding patterns, excluding initializer expressions."""

    BRACKETS = {'(': ')', '[': ']', '{': '}', '${': '}'}

    def __init__(self, closing):
        self._frames = [BindingFrame(closing)]
        self._expression_brackets = []
        self.previous = ''
        self.defaults = 0

    @property
    def in_expression(self):
        return self._frames[-1].initializing or bool(self._expression_brackets)

    @property
    def at_start(self):
        return not self.previous

    def __call__(self, token):
        return self.read_token(token).ended

    def read_token(self, token):
        if not self._frames:
            return BindingEvent(True, False, False, False, False)
        frame = self._frames[-1]
        initializing = self.in_expression
        initializer_ended = (initializing and not self._expression_brackets
                             and token in (',', frame.closing))
        root_separator = (token == ',' and len(self._frames) == 1
                          and not self._expression_brackets)
        defaults = self.defaults
        if self._expression_brackets:
            if token in self.BRACKETS:
                self._expression_brackets.append(self.BRACKETS[token])
            elif token == self._expression_brackets[-1]:
                self._expression_brackets.pop()
        elif token == frame.closing:
            self._frames.pop()
        elif token == ',':
            frame.initializing = False
        elif frame.initializing or token == '(' or (
                token == '[' and frame.closing == '}'
                and self.previous in ('', '{', ',')):
            # Computed object keys and parentheses contain expressions.
            if token in self.BRACKETS:
                self._expression_brackets.append(self.BRACKETS[token])
        elif token in ('[', '{'):
            self._frames.append(BindingFrame(']' if token == '[' else '}'))
        elif token == '=':
            self.defaults += 1
            frame.initializing = True
        ended = not self._frames
        if not ended:
            self.previous = token
        return BindingEvent(ended, initializing, self.defaults != defaults,
                            initializer_ended, root_separator)


class JavaScriptBindingsMixin:
    """Keep provisional bindings with the function that owns their CCN."""

    def __init__(self, context):
        super().__init__(context)
        self._binding_candidate = None
        self._pending_bindings = None
        self._initializer_reader = None
        self._confirmed_binding_defaults = 0

    def _resolve_pending_bindings(self, token):
        pending = self._pending_bindings
        self._pending_bindings = None
        if not pending:
            return
        if token in ('=', 'of', 'in'):
            if self._binding_candidate:
                self._confirmed_binding_defaults += pending.defaults
            else:
                self.context.add_condition(pending.defaults)
        elif token == '=>':
            self._pending_bindings = pending
        else:
            self.context.add_condition(pending.confirmed_defaults)

    def _read_binding_candidate(self, closing, callback=None,
                                as_object=False, track_complexity=True):
        candidate = self.__class__(self.context)
        candidate.as_object = as_object
        candidate._binding_candidate = JavaScriptBindings(closing)
        function = self.context.current_function
        complexity = function.cyclomatic_complexity

        def complete():
            summary = BindingSummary(
                candidate._binding_candidate.defaults,
                function.cyclomatic_complexity - complexity if track_complexity else 0,
                candidate._confirmed_binding_defaults if track_complexity else 0)
            if callback:
                callback(summary)
            else:
                self._pending_bindings = summary

        self.sub_state(candidate, complete)
        return candidate

    def _push_arrow_function(self):
        pending = self._pending_bindings
        self._pending_bindings = None
        if self.started_function:
            return
        if pending:
            self.context.add_condition(-pending.expression_complexity)
        self._push_function_to_stack()
        if pending:
            self.context.add_condition(pending.defaults + pending.expression_complexity)

    def _read_parameter_binding(self, token):
        event = self._parameter_bindings.read_token(token)
        if event.initializer_started:
            self.context.add_condition()
            self._initializer_reader = self.__class__(self.context)
        elif event.in_expression:
            if event.initializer_ended:
                self._initializer_reader.statemachine_before_return()
                self._initializer_reader = None
            elif self._initializer_reader:
                self._initializer_reader(token)
        return event
