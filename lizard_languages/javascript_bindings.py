"""JavaScript binding patterns and ownership of initializer complexity."""

from collections import namedtuple
from functools import partial

from lizard_core.function_info import FunctionInfo


BindingEvent = namedtuple('BindingEvent', [
    'ended', 'in_expression', 'initializer_started',
    'initializer_ended', 'root_separator',
])
BindingSummary = namedtuple('BindingSummary', [
    'defaults', 'expression_complexity', 'confirmed_defaults', 'full_parameters',
])


class BindingFrame:
    def __init__(self, closing):
        self.closing = closing
        self.initializing = False


class OptionalCallBinding(str):
    """A bare optional callee and the scopes that can bind its name."""

    def __new__(cls, token, function, scopes):
        reference = super().__new__(cls, token)
        reference.function = function
        reference.scopes = scopes
        return reference


class JavaScriptBindings:
    """Recognize defaults in binding patterns, excluding initializer expressions."""

    BRACKETS = {'(': ')', '[': ']', '{': '}', '${': '}'}

    def __init__(self, closing, function=None, parameters=None, recursive_name=''):
        self._frames = [BindingFrame(closing)]
        self._expression_brackets = []
        self.previous = ''
        self.defaults = 0
        if function:
            self._parameters = function.full_parameters
            self._add_parameter = function.add_parameter
        else:
            self._parameters = parameters
            self._add_parameter = (
                partial(FunctionInfo._append_parameter, parameters)
                if parameters is not None else None)
        self._in_parameter_type = False
        self._generic_depth = 0
        self.recursive_name = recursive_name
        self._possible_shadow = False

    @property
    def in_expression(self):
        return bool(self._frames) and (
            self._frames[-1].initializing or bool(self._expression_brackets))

    @property
    def at_start(self):
        return not self.previous

    @property
    def at_root(self):
        return len(self._frames) == 1 and not self._expression_brackets

    def read_token(self, token, report=True):
        if not self._frames:
            return (BindingEvent(True, False, False, False, False)
                    if report else None)
        frame = self._frames[-1]
        if self._possible_shadow:
            if token != ':':
                self.recursive_name = ''
            self._possible_shadow = False
        expression_brackets = self._expression_brackets
        initializing = frame.initializing or bool(expression_brackets)
        if token == 'this' and self.at_start:
            # A TypeScript receiver declaration is not a formal argument.
            self._in_parameter_type = True
        if (self._add_parameter and token in ('{', '[') and self.at_root
                and not initializing and not self._in_parameter_type):
            # Keep the formal pattern even if it contributes no binding name.
            self._add_parameter('{}' if token == '{' else '[]')
        initializer_ended = (report and initializing
                             and not expression_brackets
                             and token in (',', frame.closing))
        root_separator = (token == ',' and len(self._frames) == 1
                          and not expression_brackets)
        defaults = self.defaults
        if expression_brackets:
            if token in self.BRACKETS:
                expression_brackets.append(self.BRACKETS[token])
            elif token == expression_brackets[-1]:
                expression_brackets.pop()
        elif token == frame.closing:
            self._frames.pop()
        elif token == ',':
            frame.initializing = False
        elif frame.initializing or token in ('(', '${') or (
                token == '[' and frame.closing == '}'
                and self.previous in ('', '{', ',')):
            # Computed keys, parentheses and template substitutions are expressions.
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
        if self._add_parameter and not ended:
            self._read_parameter(token, initializing, root_separator)
        if report:
            return BindingEvent(ended, initializing, self.defaults != defaults,
                                initializer_ended, root_separator)
        return None

    def _read_parameter(self, token, in_expression, root_separator):
        """Named and provisional signatures use the same FunctionInfo path."""
        if (in_expression and not root_separator) or token == '(':
            return
        if token == ',':
            if root_separator and not self._generic_depth:
                self._in_parameter_type = False
                if self._parameters:
                    self._add_parameter(',')
        elif token == ':' and self.at_root:
            self._in_parameter_type = True
        elif token == '<':
            self._generic_depth += 1
        elif token == '>':
            if self._generic_depth:
                self._generic_depth -= 1
        elif token not in ('*', '+', '-', '/', '%', '=', '.') and not (
                self._generic_depth or self._in_parameter_type):
            name = token.replace('?', '')
            if name.isidentifier() or (
                    '$' in name and name.replace('$', '_').isidentifier()):
                self._add_parameter(name)
                if name == self.recursive_name:
                    if self._frames[-1].closing == '}':
                        # A colon makes this token a property key.
                        self._possible_shadow = True
                    else:
                        self.recursive_name = ''


class JavaScriptBindingsMixin:
    """Keep provisional bindings with the function that owns their CCN."""

    _recursive_local_pattern = None

    def __init__(self, context):
        super().__init__(context)
        self._binding_candidate = None
        self._pending_bindings = None
        self._initializer_reader = None
        self._confirmed_binding_defaults = 0

    def recursive_local_scope(self):
        shadows = getattr(self, '_recursive_local_shadows', None)
        if shadows is None:
            shadows = self._recursive_local_shadows = set()
        return shadows

    def _record_recursive_local(self, token, kind):
        function = self.context.current_function
        if function not in getattr(self.context, 'bare_recursive_functions', ()):
            return
        name = function.unqualified_name
        if token == name:
            self._shadow_recursive_function(function, kind)
        elif token in ('{', '['):
            binding = JavaScriptBindings('', parameters=[], recursive_name=name)
            binding.read_token(token, report=False)
            self._recursive_local_pattern = binding, function, kind

    def _read_recursive_local_pattern(self, token):
        binding, function, kind = self._recursive_local_pattern
        binding.read_token(token, report=False)
        if binding.at_root:
            if not binding.recursive_name:
                self._shadow_recursive_function(function, kind)
            self._recursive_local_pattern = None

    def _shadow_recursive_function(self, function, kind):
        shadows = (self.context.recursive_var_shadows if kind == 'var'
                   else self.recursive_local_scope())
        shadows.add(function)

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
        elif token in ('=>', ':'):
            self._pending_bindings = pending
        else:
            self.context.add_condition(pending.confirmed_defaults)

    def _read_binding_candidate(self, closing, callback=None,
                                as_object=False, track_complexity=True):
        candidate = self.__class__(self.context)
        candidate.as_object = as_object
        parameters = [] if closing == ')' else None
        candidate._binding_candidate = JavaScriptBindings(
            closing, parameters=parameters)
        function = self.context.current_function
        complexity = function.cyclomatic_complexity

        def complete():
            summary = BindingSummary(
                candidate._binding_candidate.defaults,
                function.cyclomatic_complexity - complexity if track_complexity else 0,
                candidate._confirmed_binding_defaults if track_complexity else 0,
                parameters if parameters is not None else [])
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
            self.context.current_function.full_parameters = pending.full_parameters
            self.context.add_condition(pending.defaults + pending.expression_complexity)

    def _read_parameter_binding(self, token):
        bindings = self._parameter_bindings
        event = bindings.read_token(token)
        if event.ended and bindings.recursive_name:
            self.context.bare_recursive_functions.add(self.context.current_function)
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
