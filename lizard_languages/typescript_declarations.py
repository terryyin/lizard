"""TypeScript declarations and type syntax excluded from runtime functions."""

from .typescript_type_annotation import TypeScriptTypeAnnotationStates
from .javascript_expression import OPERAND_WORDS, ends_expression, is_identifier, is_identifier_part
from .js_style_regex_context import _EXPRESSION_KEYWORDS


# Words that start what _handle_type_declaration handles; after `declare`
# it also handles the next token.
DECLARATION_WORDS = frozenset((
    'declare', 'type', 'interface', 'abstract', 'class', 'const', 'let', 'var',
    'as', 'satisfies'))
# Type-only contextual words can be identifiers in runtime operand position.
_EXPRESSION_PREFIXES = (
    _EXPRESSION_KEYWORDS | OPERAND_WORDS | {'default'}
) - {'as', 'satisfies', 'keyof'}


class SplitTypeAliasName(str):
    """The 'type' keyword before a name the lexer split into joined parts."""


class TypeScriptDeclarationsMixin:
    def _handle_type_declaration(self, token):
        if token in ('const', 'let', 'var'):
            self._declaration_binding = True
            self.last_tokens = self._prev_token = token
            return True
        elif token == 'class' and (not self.as_object or self._in_prop_value):
            self._class_body_pending = True
        elif (token in ('as', 'satisfies')
              and (not self.as_object or self._in_prop_value)
              and self._assertion_operand()):
            self._consume_type_annotation()
            return True
        if token == 'declare':
            self._ts_declare = True
            return True
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
            return True
        self._ts_declare = False

        # Skip type alias declarations: type Name = { ... }
        # These contain arrow signatures that are not runtime functions.
        if token == 'type' and not self.as_object:
            name = []
            split_name = isinstance(token, SplitTypeAliasName)

            def handle_type_alias(t):
                if not name:
                    if not self.context.newline and (is_identifier(t) or split_name and t == '\\'):
                        name.append(t)
                    else:
                        self.last_tokens = 'type'
                        self.next(self._state_global)
                        self._state_global(t)
                        return True
                else:
                    if t == '<':
                        self._consume_generic_type_params(handle_type_alias)
                    elif t.startswith('<') and t.endswith('>'):
                        return False  # One packed type parameter list in TSX.
                    elif t == '=':
                        self.next(self._state_global)
                        self._consume_type_alias_body()
                    elif t == ';':
                        self.next(self._state_global)
                        return True
                    elif split_name and not self.context.newline and _continues_alias_name(name, t):
                        name.append(t)
                    else:
                        # `type` was a value, e.g. `f(type as string)`:
                        # an alias name is followed only by '<' or '='.
                        self.last_tokens = 'type'
                        self.next(self._state_global)
                        for part in name:
                            self._replay_token(part)
                        self._state_global(t)
                        return True
                return False

            self.next(handle_type_alias)
            return True

        # Skip interface declarations — method signatures are not runtime functions
        if token == 'interface':
            brace_count = 0
            interface_started = False

            def skip_interface(t):
                nonlocal brace_count, interface_started
                # A template literal type's '${' closes with '}' too.
                if t in ('{', '${'):
                    interface_started = interface_started or t == '{'
                    brace_count += 1
                elif t == '}' and brace_count:
                    brace_count -= 1
                    if brace_count == 0 and interface_started:
                        self.next(self._state_global)
                        return True
                return False

            self.next(skip_interface)
            return True

        # Track abstract modifier inside class bodies
        if token == 'abstract' and self.as_object:
            self._in_abstract_context = True
            return True
        return False

    def _consume_type_alias_body(self):
        annotation = TypeScriptTypeAliasStates(self.context)

        def resume():
            if annotation.saved_token:
                self._replay_token(annotation.saved_token)

        self.sub_state(annotation, resume)

    def _assertion_operand(self):
        """Assertions follow consumed operands, not expression/statement starts."""
        if (self.context.newline or not self._prev_token
                or self._declaration_binding is True or self._class_body_pending):
            return False
        token = self.last_token or ''
        if token == '!':
            return getattr(self, '_postfix_operand', False)
        return ((self._prev_token == '.' or token not in _EXPRESSION_PREFIXES)
                and (ends_expression(token) or token.isidentifier()))

    def _consume_generic_type_params(self, resume=None):
        """Consume <...> generic type parameters (e.g., method<T>(...))
        so the method name in last_tokens is preserved; then continue in
        resume (the global state by default)."""
        depth = 1
        braces = 0

        def consume(token):
            nonlocal depth, braces
            if token == '{':
                braces += 1
            elif token == '}' and braces:
                braces -= 1
            elif token in ('}', ';') and not braces:
                # A comparison, not type parameters: `{i < n}`. Object
                # types inside them (`Promise<{ a: T; }>`) keep their own.
                self.next(resume or self._state_global, token)
                return
            if token == '<':
                depth += 1
            elif token == '>':
                depth -= 1
                if depth == 0:
                    self.next(resume or self._state_global)
        self.next(consume)

    def _consume_type_annotation(self):
        typeStates = TypeScriptTypeAnnotationStates(self.context)

        def callback():
            if typeStates.saved_token:
                self._replay_token(typeStates.saved_token)
        self.sub_state(typeStates, callback)

    def _read_runtime_colon(self):
        binding = self._binding_candidate
        if (self._declaration_binding is True or binding
                and binding._parameters is not None
                and binding.at_root and not binding.in_expression):
            self._typed_parameters = True
            self._consume_type_annotation()
        else:
            # An unclaimed runtime colon introduces a labeled statement.
            self.next(self._expecting_statement_or_block)
        self._prev_token = ':'


def has_split_alias_name(tokens):
    """Check the alias name and its delimiter with the original whitespace."""
    name = []
    gap = False
    for token in tokens:
        if token.isspace() or token.startswith(('//', '/*')):
            if not name and '\n' in token:
                return False
            gap = True
        elif not name:
            if not gap or not (token == '\\' or is_identifier(token)):
                return False
            name.append(token)
            gap = False
        elif token in ('<', '=') or token.startswith('<') and token.endswith('>'):
            return len(name) > 1
        elif gap or not _continues_alias_name(name, token):
            return False
        else:
            name.append(token)
    return False


def _continues_alias_name(name, token):
    """The tokenizer emits identifier escapes and some Unicode parts separately."""
    return (token == '\\' or is_identifier_part(token)
            or token in ('{', '}') and '\\' in name)


class TypeScriptTypeAliasStates(TypeScriptTypeAnnotationStates):
    """An alias consumes function types and stops before runtime calls."""

    def __init__(self, context):
        super().__init__(context)
        self._after_type_parameters = False
        self._import_type = False

    def _state_global(self, token):
        if not self._brackets and not token.isspace():
            if not self._expect_type and self._starts_runtime_expression(token):
                self.saved_token = token
                self.statemachine_return()
                return
            self._after_type_parameters = self._expect_type and token[0] == '<'
            self._import_type = token == 'import' and not self._qualified_type
            if (token == 'new' and not self._qualified_type
                    and (self._expect_type or self.last_token == 'abstract')):
                # A constructor type has its own parameters and return arrow.
                self._expect_type = True
                return
            if self._qualified_type:
                self._expect_type = True
        super()._state_global(token)

    def _starts_runtime_expression(self, token):
        if token == '(':
            return not (self._after_type_parameters or self._import_type)
        # A line break ends a type before postfix brackets or type arguments.
        return self.context.newline and token[0] in '[<'
