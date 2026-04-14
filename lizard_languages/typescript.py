'''
Language parser for TypeScript
'''

import re
from .code_reader import CodeReader, CodeStateMachine
from .clike import CCppCommentsMixin
from .js_style_regex_expression import js_style_regex_expression


class Tokenizer(object):
    def __init__(self):
        self.sub_tokenizer = None
        self._ended = False

    def __call__(self, token):
        if self.sub_tokenizer:
            for tok in self.sub_tokenizer(token):
                yield tok
            if self.sub_tokenizer._ended:
                self.sub_tokenizer = None
            return
        for tok in self.process_token(token):
            yield tok

    def stop(self):
        self._ended = True

    def process_token(self, token):
        pass


class JSTokenizer(Tokenizer):
    def __init__(self):
        super().__init__()
        self.depth = 1

    def process_token(self, token):
        if token == "{":
            self.depth += 1
        elif token == "}":
            self.depth -= 1
            if self.depth == 0:
                self.stop()
                return
        yield token


class TypeScriptReader(CodeReader, CCppCommentsMixin):
    # pylint: disable=R0903

    ext = ['ts']
    language_names = ['typescript', 'ts']

    # Separated condition categories
    _control_flow_keywords = {'if', 'elseif', 'for', 'while', 'catch'}
    _logical_operators = {'&&', '||'}
    _case_keywords = {'case'}
    _ternary_operators = {'?'}

    def __init__(self, context):
        super().__init__(context)
        self.parallel_states = [TypeScriptStates(context)]

    @staticmethod
    @js_style_regex_expression
    def generate_tokens(source_code, addition='', token_class=None):
        def split_template_literal(token, quote):
            content = token[1:-1]

            # Always yield opening quote
            yield quote

            # If no expressions, yield content as-is with quotes and closing quote
            if '${' not in content:
                if content:
                    yield quote + content + quote
                yield quote
                return

            # Handle expressions
            i = 0
            while i < len(content):
                idx = content.find('${', i)
                if idx == -1:
                    if i < len(content):
                        yield quote + content[i:] + quote
                    break
                if idx > i:
                    yield quote + content[i:idx] + quote
                yield '${'
                i = idx + 2
                expr_start = i
                brace_count = 1
                while i < len(content) and brace_count > 0:
                    if content[i] == '{':
                        brace_count += 1
                    elif content[i] == '}':
                        brace_count -= 1
                    i += 1
                expr = content[expr_start:i - 1]
                yield expr
                yield '}'
                content = content[i:]
                i = 0

            # Always yield closing quote
            yield quote

        # Private method (#), dollar ($), optional chaining (?), template literals
        addition = addition + r"|(?:#\w+)" + r"|(?:\$\w+)" + r"|(?:\w+\?)" + r"|`.*?`"
        for token in CodeReader.generate_tokens(source_code, addition, token_class):
            if (
                isinstance(token, str)
                and token.startswith('`')
                and token.endswith('`')
                and len(token) > 1
            ):
                for t in split_template_literal(token, '`'):
                    yield t
                continue
            yield token


# TypeScript type keywords that should not be counted as parameters
_TS_TYPE_KEYWORDS = frozenset([
    'string', 'number', 'boolean', 'void', 'any',
    'object', 'unknown', 'never',
])


class TypeScriptStates(CodeStateMachine):
    def __init__(self, context):
        super().__init__(context)
        self.last_tokens = ''
        self.function_name = ''
        self.started_function = None
        self.as_object = False
        self._getter_setter_prefix = None
        self.arrow_function_pending = False
        self._ts_declare = False  # Track if 'declare' was seen
        self._static_seen = False  # Track if 'static' was seen
        self._async_seen = False  # Track if 'async' was seen
        self._prev_token = ''  # Track previous token to detect method calls
        self._in_prop_value = False  # Track if inside property value (after ':')
        self._in_abstract_context = False  # Track abstract method declarations

    def statemachine_before_return(self):
        # Ensure the main function is closed at the end
        if self.started_function:
            self._pop_function_from_stack()

    def _state_global(self, token):
        if token == 'declare':
            self._ts_declare = True
            return
        if token == 'function' and getattr(self, '_ts_declare', False):
            # Skip declared function
            self._ts_declare = False
            # Skip tokens until semicolon or newline

            def skip_declared_function(t):
                if t == ';' or self.context.newline:
                    self.next(self._state_global)
                    return True
                return False
            self.next(skip_declared_function)
            return
        self._ts_declare = False

        # Skip type alias declarations: type Name = { ... }
        # These contain arrow signatures that are not runtime functions.
        if token == 'type' and not self.as_object:
            phase = [0]        # 0=expect name, 1=expect =, 2=after =
            brace_count = [0]
            generic_depth = [0]

            def handle_type_alias(t):
                if phase[0] == 0:
                    if t and t[0].isalpha():
                        phase[0] = 1
                    else:
                        self.last_tokens = 'type'
                        self.next(self._state_global)
                        self._state_global(t)
                        return True
                elif phase[0] == 1:
                    if t == '<':
                        generic_depth[0] = 1
                        phase[0] = 3
                    elif t == '=':
                        phase[0] = 2
                    elif t == ';':
                        self.next(self._state_global)
                        return True
                elif phase[0] == 2:
                    if t == '{':
                        brace_count[0] = 1
                        phase[0] = 4
                    elif t == ';' or self.context.newline:
                        self.next(self._state_global)
                        if t != ';':
                            self._state_global(t)
                        return True
                elif phase[0] == 3:
                    if t == '<':
                        generic_depth[0] += 1
                    elif t == '>':
                        generic_depth[0] -= 1
                        if generic_depth[0] == 0:
                            phase[0] = 1
                elif phase[0] == 4:
                    if t == '{':
                        brace_count[0] += 1
                    elif t == '}':
                        brace_count[0] -= 1
                        if brace_count[0] == 0:
                            self.next(self._state_global)
                            return True
                return False

            self.next(handle_type_alias)
            return

        # Skip interface declarations — method signatures are not runtime functions
        if token == 'interface':
            brace_count = 0
            interface_started = False

            def skip_interface(t):
                nonlocal brace_count, interface_started
                if t == '{':
                    interface_started = True
                    brace_count += 1
                elif t == '}' and interface_started:
                    brace_count -= 1
                    if brace_count == 0:
                        self.next(self._state_global)
                        return True
                return False

            self.next(skip_interface)
            return

        # Track abstract modifier inside class bodies
        if token == 'abstract' and self.as_object:
            self._in_abstract_context = True
            return

        # Track static and async modifiers
        if token == 'static':
            self._static_seen = True
            self._prev_token = token
            return
        if token == 'async':
            self._async_seen = True
            self._prev_token = token
            return
        if token == 'new':
            # Track 'new' keyword to avoid treating constructors as functions
            self._prev_token = token
            return

        if self.as_object:
            # Support for getter/setter: look for 'get' or 'set' before method name
            if token in ('get', 'set'):
                self._getter_setter_prefix = token
                return
            if self._getter_setter_prefix:
                # Next token is the property name
                self.last_tokens = f"{self._getter_setter_prefix} {token}"
                self._getter_setter_prefix = None
                return
            if token == '[':
                self._collect_computed_name()
                return
            if token == ':':
                # Only set function_name for valid identifiers
                name = self.last_tokens
                if name and (name[0].isalpha() or name[0] in ('_', '$', '#')):
                    self.function_name = name
                self._in_prop_value = True
                return
            elif token == '<' or (
                    token.startswith('<') and token.endswith('>') and len(token) > 1):
                # Generic type params on method: sortByKey<T>(...) {
                # Handles both multi-token <T, U> and single-token <T> from TSX tokenizer.
                if token == '<':
                    self._consume_generic_type_params()
                return
            elif token == '(':
                # Check if this is a method call (previous token was . or new)
                if self._prev_token == '.' or self._prev_token == 'new':
                    # Method call inside object — use sub_state so
                    # the matching ')' doesn't escape the object reader.
                    self.sub_state(self.__class__(self.context))
                    self._prev_token = token
                    return
                # In property value (after ':'), identifier( is a function call
                # unless it's the prop name itself: prop: (...) => {} is arrow fn
                if self._in_prop_value and (
                        not self.function_name
                        or self.last_tokens != self.function_name):
                    self.sub_state(self.__class__(self.context))
                    self._prev_token = token
                    return
                if not self.started_function:
                    self.arrow_function_pending = True
                    # When last_tokens is '=' we're in a field assignment
                    # pattern (field = () => {}), so use function_name which
                    # was set by the '=' handler to the field name.
                    if self.last_tokens == '=' and self.function_name:
                        self._function(self.function_name)
                    else:
                        self._function(self.last_tokens)
                self.next(self._function, token)
                return
            # If we've seen async/static and this is an identifier, it's likely a method name
            elif (self._async_seen or self._static_seen) and token not in ('*', 'function'):
                if token == '=':
                    # End of static/async field name — clear modifiers so
                    # the value expression and subsequent members parse
                    # normally.  e.g. `static propTypes = { ... };`
                    self._static_seen = False
                    self._async_seen = False
                    # Fall through to the general '=' handler below
                else:
                    # This is a method name after async/static
                    self.last_tokens = token
                    return

        if token == '.':
            self._state = self._field
            self.last_tokens += token
            self._prev_token = token
            return
        if token == 'function':
            self._state = self._function
        elif token in ('if', 'switch', 'for', 'while', 'catch'):
            self.next(self._expecting_condition_and_statement_block)
        elif token in ('else', 'do', 'try', 'final'):
            self.next(self._expecting_statement_or_block)
        elif token in ('=>',):
            self._state = self._arrow_function
        elif token == '=':
            # Only set function_name for valid identifiers
            name = self.last_tokens
            if name and (name[0].isalpha() or name[0] in ('_', '$', '#')):
                self.function_name = name
        elif token == "(":
            # Check if this is a method call or constructor
            if self._prev_token == '.' or self._prev_token == 'new':
                # This is a method call or constructor, not a function definition
                self.sub_state(
                    self.__class__(self.context))
            elif self.function_name:
                # Distinguish arrow-function definition from function call:
                #   const fn = (...) => {}   <- _prev_token is '=' or 'async'
                #   const fn = someFunc(...)  <- _prev_token is an identifier
                # In the second case, ( follows an identifier that differs
                # from function_name, so it's a call — not a definition.
                if (self.last_tokens != self.function_name
                        and self._prev_token not in ('=', 'async', '>')):
                    self.function_name = ''
                    self.sub_state(self.__class__(self.context))
                else:
                    if not self.started_function:
                        self.arrow_function_pending = True
                        self._function(self.function_name)
                    self.next(self._function, token)
            else:
                self.sub_state(
                    self.__class__(self.context))
        elif token == '{':
            if self.started_function:
                self.sub_state(
                    self.__class__(self.context),
                    self._pop_function_from_stack)
            else:
                self.read_object()
        elif token in ('}', ')'):
            self.statemachine_return()
        elif self.context.newline or token == ';':
            self.function_name = ''
            self._pop_function_from_stack()
            # Reset modifiers on newline/semicolon
            self._static_seen = False
            self._async_seen = False
            self._in_abstract_context = False
            self._in_prop_value = False

        if token == '`':
            self.next(self._state_template_literal)
        if not self.as_object:
            if token == ':':
                self._consume_type_annotation()
                self._prev_token = token
                return
        if self.as_object and token == ',':
            self._in_prop_value = False
        self.last_tokens = token
        # Don't overwrite _prev_token if it's 'new' or '.' (preserve for next token)
        if self._prev_token not in ('new', '.'):
            self._prev_token = token

    def read_object(self):
        def callback():
            self.next(self._state_global)

        object_reader = self.__class__(self.context)
        object_reader.as_object = True
        # Pass along the modifier flags
        object_reader._static_seen = self._static_seen
        object_reader._async_seen = self._async_seen
        self.sub_state(object_reader, callback)
        # Reset modifiers after entering object
        self._static_seen = False
        self._async_seen = False

    def _expecting_condition_and_statement_block(self, token):
        def callback():
            self.next(self._expecting_statement_or_block)

        if token == "await":
            return

        if token != '(':
            self.next(self._state_global, token)
            return

        self.sub_state(
            self.__class__(self.context), callback)

    def _expecting_statement_or_block(self, token):
        def callback():
            self.next(self._state_global)
        if token == "{":
            self.sub_state(
                self.__class__(self.context), callback)
        else:
            self.next(self._state_global, token)

    def _push_function_to_stack(self):
        if self._in_abstract_context:
            return
        self.started_function = True
        self.context.push_new_function(self.function_name or '(anonymous)')

    def _pop_function_from_stack(self):
        if self.started_function:
            self.context.end_of_function()
        self.started_function = None
        self._in_prop_value = False

    def _arrow_function(self, token):
        if not self.started_function:
            self._push_function_to_stack()
        # Clear function_name so expression-body ( doesn't re-enter _function
        self.function_name = ''
        self.next(self._state_global, token)

    def _function(self, token):
        if token == '*':
            return
        if token == '<':
            # Generic type params: function name<T>(...) — consume <...>
            # so function_name (already set) is preserved.
            self._consume_generic_type_params()
            return
        if token.startswith('<') and token.endswith('>') and len(token) > 1:
            # Single-token generic from TSX tokenizer (e.g., <T>, <Props>)
            return
        if token != '(':
            # Only set function_name for valid identifiers
            if token and (token[0].isalpha() or token[0] in ('_', '$', '#')):
                self.function_name = token
            else:
                self.function_name = ''
            # Reset modifiers after setting function name
            self._static_seen = False
            self._async_seen = False
        else:
            if not self.started_function:
                self._push_function_to_stack()
            self.arrow_function_pending = False
            self._generic_depth_in_dec = 0
            self._state = self._dec
            if token == '(':
                self._dec(token)

    def _field(self, token):
        self.last_tokens += token
        self._state = self._state_global

    def _dec(self, token):
        if token == ')':
            self._state = self._expecting_func_opening_bracket
        elif token != '(':
            # Filter out TypeScript type keywords and operators from parameter count
            if token == ',':
                # Ignore commas inside generic type brackets: Map<K, V>
                if not getattr(self, '_generic_depth_in_dec', 0):
                    self.context.parameter(',')
            elif token == '<':
                self._generic_depth_in_dec = getattr(
                    self, '_generic_depth_in_dec', 0) + 1
            elif token == '>':
                depth = getattr(self, '_generic_depth_in_dec', 0)
                if depth > 0:
                    self._generic_depth_in_dec = depth - 1
            elif token in _TS_TYPE_KEYWORDS:
                pass
            elif token in ('*', '+', '-', '/', '%', '=', '.'):
                pass
            elif not getattr(self, '_generic_depth_in_dec', 0):
                if (token.replace('_', '').replace('?', '').isalnum() and
                        token.replace('?', '') and
                        token.replace('?', '')[0].isalpha()):
                    self.context.parameter(token.replace('?', ''))
            return
        self.context.add_to_long_function_name(" " + token)

    def _expecting_func_opening_bracket(self, token):
        # Do not reset started_function for arrow functions (=>)
        if token == ':':
            self._consume_type_annotation()
        elif token == ';' and self.as_object and self._in_abstract_context:
            # Abstract method declaration ends with ';' — no body
            if self.started_function:
                self._pop_function_from_stack()
            self._in_abstract_context = False
            self.next(self._state_global)
        elif token != '{' and token != '=>':
            if self.started_function:
                # Arrow function not confirmed (no => or { after params).
                # Use forgive to cleanly un-push the optimistic function
                # so it doesn't appear in output or corrupt the stack.
                self.context.forgive = True
                self.context.end_of_function()
            self.started_function = None
        self.next(self._state_global, token)

    def _state_template_literal(self, token):
        if token == '`':
            self.next(self._state_global)

    def _collect_computed_name(self):
        # Collect tokens between [ and ]
        tokens = []

        def collect(token):
            if token == ']':
                # Try to join tokens and camelCase if possible
                name = ''.join(tokens)
                # Remove quotes and pluses for simple cases
                name = name.replace("'", '').replace('"', '').replace('+', '').replace(' ', '')
                # Lowercase first char, uppercase next word's first char
                name = self._to_camel_case(name)
                self.last_tokens = name
                self.next(self._state_global)
                return True
            tokens.append(token)
            return False
        self.next(collect)

    def _to_camel_case(self, s):
        # Simple camelCase conversion for test case
        if not s:
            return s
        parts = s.split()
        if not parts:
            return s
        return parts[0][0].lower() + parts[0][1:] + ''.join(p.capitalize() for p in parts[1:])

    def _consume_generic_type_params(self):
        """Consume <...> generic type parameters (e.g., method<T>(...))
        so the method name in last_tokens is preserved."""
        depth = 1

        def consume(token):
            nonlocal depth
            if token == '<':
                depth += 1
            elif token == '>':
                depth -= 1
                if depth == 0:
                    self.next(self._state_global)
        self.next(consume)

    def _consume_type_annotation(self):
        typeStates = TypeScriptTypeAnnotationStates(self.context)

        def callback():
            if typeStates.saved_token:
                self(typeStates.saved_token)
        self.sub_state(typeStates, callback)


class TypeScriptTypeAnnotationStates(CodeStateMachine):
    def __init__(self, context):
        super().__init__(context)
        self.saved_token = None

    def _state_global(self, token):
        if token == '{':
            self.next(self._inline_type_annotation, token)
        else:
            self.next(self._state_simple_type, token)

    def _state_simple_type(self, token):
        if token == '<':
            self.next(self._state_generic_type, token)
        elif token in '{=;)':
            self.saved_token = token
            self.statemachine_return()
        elif token == '(':
            self.next(self._function_type_annotation, token)
        elif token == '=>':
            # Handle arrow function after type annotation
            self.saved_token = token
            self.statemachine_return()

    @CodeStateMachine.read_inside_brackets_then("{}")
    def _inline_type_annotation(self, _):
        self.statemachine_return()

    @CodeStateMachine.read_inside_brackets_then("<>")
    def _state_generic_type(self, token):
        self.statemachine_return()

    @CodeStateMachine.read_inside_brackets_then("()")
    def _function_type_annotation(self, _):
        self.statemachine_return()
