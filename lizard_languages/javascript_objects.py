"""JavaScript object and class members, including computed method names."""

from .javascript_expression import active_js_state, ends_expression
from .javascript_statements import opens_object

# The tokens before the name of a method in an object literal; it reads
# the first member with no token before it.
_MEMBER_STARTS = frozenset((None, ',', '*', 'async', 'get', 'set'))
_CLASS_MEMBER_STARTS = frozenset((
    None, ';', '}', 'static', 'async', 'get', 'set', 'public', 'protected',
    'private', 'abstract', 'declare', 'override', 'readonly', 'accessor'))


class JavaScriptObjectsMixin:
    # A class body or an object literal: braces that hold members.  The
    # reader also reads blocks such as `catch {` or `case 1: {` as objects.
    holds_members = False
    # In an object: a default value is open, `{ a = async function () {} }`.
    _default_value = False
    _class_async_modifier = False

    def _typed_class_member(self):
        return (self.as_class and not self._in_prop_value
                and self.context.class_field_annotations)

    def optional_member_context(self):
        state = active_js_state(self)
        return state._state == state._state_global and state._typed_class_member()

    def member_name_context(self):
        """Whether a word before '(' here names a method, `catch(e) {}`."""
        state = active_js_state(self)
        return (state.holds_members and state._state == state._state_global
                and not state._in_prop_value and not state._default_value
                and (state.as_class or state.last_token in _MEMBER_STARTS))

    def _end_field_before_modifier(self):
        """A modifier at a line start begins the next class member, so a
        field value written without ';' ends before it, as before a name."""
        if (self.as_class and self._in_prop_value and self.context.newline
                and ends_expression(self.last_token)):
            self._end_statement()

    def _optional_class_member(self):
        if not self._typed_class_member():
            return False
        self.context.add_condition(-1)
        return True

    def _starts_class_member(self):
        return (self.last_token in _CLASS_MEMBER_STARTS
                or self.context.newline and ends_expression(self.last_token))

    def _read_class_new(self, token, after_field_type=False):
        """Whether "new" names a class member outside a field value."""
        after_non_null = (self.context.newline and self.last_token == '!'
                          and getattr(self, '_postfix_operand', False))
        if self._in_prop_value:
            if after_non_null:
                self._end_statement()
            else:
                self._end_field_before_modifier()
            if self._in_prop_value:
                return False
        elif not (after_field_type or after_non_null or self._starts_class_member()):
            # A multiline initializer may already have cleared the field flag.
            if not (self.last_token == '*' and (
                    self._static_seen or self._async_seen and self._class_async_modifier)):
                return False
        if self.started_function:
            if self._expression_body:
                self._pop_function_from_stack()
            else:
                self._discard_signature()
        if not self._handle_object_member(token):
            self.last_tokens = token
        self._prev_token = ''
        return True

    def _handle_object_member(self, token):
        """Called for tokens in an object literal or class body (as_object)."""
        if (self._async_seen or self._static_seen) and token in (
                ':', ',', '}', ';'):
            # `{async: v}`, `{static, x}`: the word is a property name,
            # not a modifier waiting for a method name.
            self.last_tokens = 'async' if self._async_seen else 'static'
            self._async_seen = self._static_seen = False
        if token == '=':
            if self.as_class:
                self._in_prop_value = True
            else:
                self._default_value = True
        if (self.as_class and token.endswith('?')
                and self._typed_class_member()):
            self.last_tokens = token[:-1]
            return True
        # Support for getter/setter: look for 'get' or 'set' before method name
        if token == '...':
            # What a spread takes is a value: ...(c && {f() {}}), ...g(x)
            self._in_prop_value = True
            self.last_tokens = token
            return True
        if token in ('get', 'set'):
            # Only relevant while a semicolon-less field value is open; skip
            # the helper on the common path (method/getter declarations).
            if self._in_prop_value:
                self._end_field_before_modifier()
            self._getter_setter_prefix = token
            return True
        if self._getter_setter_prefix:
            prefix, self._getter_setter_prefix = self._getter_setter_prefix, None
            if token not in ('(', ':', ',', '}', '=', ';', '<'):
                # Next token is the property name
                self.last_tokens = f"{prefix} {token}"
                return True
            # A member named "get" or "set": get(key) {}, {get: f}, {get, set}
            self.last_tokens = prefix
        if token == '[':
            if self._in_prop_value:
                # An array value, {list: [[1], 2]}, not a computed name.
                self._read_binding_candidate(']')
            else:
                self._collect_computed_name()
            return True
        if token == ':' and self.as_class and self._typed_class_member():
            self.function_name = self.last_tokens
            self._consume_type_annotation(class_field=True)
            return True
        if token == ':':
            # Only set function_name for valid identifiers
            name = self.last_tokens
            if name and (name[0].isalpha() or name[0] in ('_', '$', '#')):
                self.function_name = name
            self._in_prop_value = True
            return True
        elif (token == '<' or token.startswith('<') and token.endswith('>')) and (
                not self._in_prop_value or self.last_token in (':', '=')):
            # Methods and the start of arrow values can have type parameters.
            # Inside a value, an infix '<' is not a method signature.
            if token == '<':
                self._consume_generic_type_params()
            return True
        elif token == '(':
            # Check if this is a method call (previous token was . or new)
            if self._prev_token == '.' or self._prev_token == 'new':
                # Method call inside object — use its group reader so
                # the matching ')' doesn't escape the object reader.
                self._read_parenthesis_group()
                self._prev_token = token
                return True
            # In property value (after ':'), identifier( is a function call
            # unless it's the prop name itself: prop: (...) => {} is arrow fn
            if self._in_prop_value and self.last_tokens != '=' and (
                    not self.function_name
                    or self.last_tokens != self.function_name):
                self._read_parenthesis_group()
                self._prev_token = token
                return True
            # Only "field = (" and "key: (" can open a group; a "(" after
            # a member name opens parameters.
            self._signature = self.last_token not in ('=', ':')
            if not self.started_function:
                # When last_tokens is '=' we're in a field assignment
                # pattern (field = () => {}), so use function_name which
                # was set by the '=' handler to the field name.
                if self.last_tokens == '=' and self.function_name:
                    self._function(self.function_name)
                else:
                    self._function(self.last_tokens)
            self.next(self._function, token)
            return True
        elif token == '{' and self._static_seen and self.as_class:
            # A static initialization block holds statements.
            self._static_seen = False
            self.sub_state(self.__class__(self.context))
            return True
        # If we've seen async/static and this is an identifier, it's likely a method name
        elif (self._async_seen or self._static_seen) and token not in ('*', 'function', '=>'):
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
                return True
        return False

    def read_object(self):
        def callback(summary):
            self._pending_bindings = summary
            self.next(self._state_global)

        object_reader = self._read_binding_candidate(
            '}', callback, as_object=True, track_complexity=False)
        object_reader.as_class = self._class_body_pending
        object_reader.holds_members = self._opens_members()
        self._class_body_pending = False
        object_reader._static_seen = self._static_seen
        object_reader._async_seen = self._async_seen
        self._static_seen = False
        self._async_seen = False

    def _opens_members(self):
        """Whether the '{' just read holds members, `x = {` or a class
        body, and not statements, `finally {`."""
        previous = self.last_token
        if self._class_body_pending:
            return True
        if previous is None:
            # A group's first token, `f({`, but in a block, `finally { {`.
            return not self.as_object
        if previous == ':':
            # A value, `{ a: {`, but in a block, `finally { label: {`.
            return self.holds_members
        if previous == '/':
            # The end of a regular expression can be a token of its own,
            # `"" + /a/` + line break + `{`.  Nothing divides by an object.
            return False
        if self.context.newline and (
                previous == '!' or previous[0].isalpha() or previous[0] in '_$'):
            # A "{" that starts a line after a word or a '!' is almost always
            # a block: `return`, `x!`, `[1] as const`, `factory.of`.  An
            # object there, `export default` + line break + `{`, keeps its
            # base reading, without methods named like keywords.
            return False
        # The token of a regular expression starts with a space.
        return opens_object(previous.lstrip())

    def _collect_computed_name(self):
        # Collect tokens between [ and its ]
        tokens = []
        depth = [0]

        def collect(token):
            depth[0] += {'[': 1, ']': -1}.get(token, 0)
            if depth[0] < 0:
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
