"""Consume type-owned brackets and return enclosing runtime delimiters."""

from .code_reader import CodeStateMachine
from .javascript_expression import (
    JSXMarkup, OPERAND_WORDS, is_identifier, is_identifier_part, starts_statement)


# These words introduce an expression. A following '<' starts an angle
# assertion or a generic arrow rather than specializing that word.
_EXPRESSION_PREFIXES = OPERAND_WORDS | frozenset((
    'return', 'throw', 'case', 'default', 'else', 'do', 'function',
    'const', 'let', 'var', 'export', 'async', 'extends'))
_NON_TYPE_OPERATORS = frozenset((
    '+', '*', '/', '%', '**', '&&', '||', '??', '++', '--', '!',
    '==', '!=', '===', '!==', '<=', '>=', '<<=', '>>=', '^'))
_TYPE_SEPARATORS = frozenset(('<', '>', ',', '|', '&'))
_TYPE_PREFIXES = frozenset(('<', ',', '.', '|', '&', 'keyof', 'readonly'))
# These tokens cannot be operands of a greater-than comparison.
_STATEMENTS_ONLY = frozenset((
    'if', 'else', 'for', 'while', 'do', 'switch', 'case', 'default', 'try',
    'catch', 'finally', 'break', 'continue', 'return', 'throw', 'debugger',
    'const', 'var', 'export', ')', ']', '}'))


def can_take_type_arguments(previous):
    """Whether '<' can specialize the value or type before it."""
    return previous and (previous in (')', ']', '!') or
                         (previous not in _EXPRESSION_PREFIXES
                          and (previous[0].isalpha() or previous[0] in '_$#')
                          and not isinstance(previous, JSXMarkup)))


def packed_type_arguments(token):
    """TSX can supply a simple '<Name>' as one token."""
    return (len(token) > 2 and token[1].isalpha() and token.endswith('>')
            and not isinstance(token, JSXMarkup))


def followed_by_new_statement(tokens, simple_type_arguments):
    """Check only the gap after '>', plus the first following code token."""
    newline = False
    for token in tokens:
        if token.isspace() or token.startswith(('//', '/*')):
            newline |= '\n' in token
        else:
            if not newline:
                return False
            # A balanced group may look like a type to the annotation
            # reader. Require a real statement boundary in that case.
            if not simple_type_arguments:
                return token in _STATEMENTS_ONLY
            return starts_statement(token)
    return False


class TypeScriptTypeAnnotationStates(CodeStateMachine):
    NON_GENERIC_TYPES = frozenset((
        'any', 'unknown', 'never', 'number', 'bigint', 'boolean', 'string',
        'symbol', 'object', 'void', 'undefined', 'null', 'this', 'true', 'false'))
    BRACKETS = {'(': ')', '[': ']', '{': '}', '<': '>', '${': '}'}
    CLOSERS = frozenset(BRACKETS.values())
    END_TOKENS = frozenset(('=', ';', ')', ']', '}', ',', '=>',
                            '||', '&&', '??', '?', ':', '+', '-', '*', '/',
                            '%', '**', '==', '===', '!=', '!==', '>', '>=', '<=',
                            'instanceof', 'in', '^'))

    def __init__(self, context, return_type=False, class_member=False, runtime_assertion=False):
        super().__init__(context)
        self.runtime_assertion = runtime_assertion
        self.saved_token = None
        self._brackets = []
        self._expect_type = True
        self._function_arrow = False
        self._parenthesis_first = None
        self._conditional_type = False
        self._conditional_questions = 0
        self._line_break = False
        self._closed_type = False
        self._qualified_type = False
        self._reference_parenthesis = False
        self._class_member = class_member
        self._predicate_subject = None if return_type else False
        self._assertion_subject = False
        self._assertion_predicate = False

    def owns_question_mark(self):
        return bool(self._brackets or self._conditional_type)

    def _state_global(self, token):
        if token.isspace():
            self._line_break |= '\n' in token
            return
        newline = self._line_break or self.context is not None and self.context.newline
        self._line_break = False
        brackets = self._brackets
        if brackets:
            if self._parenthesis_first is None and len(brackets) == 1:
                # A parenthesized function type groups its own arrow; the
                # arrow after the group belongs to the runtime signature.
                self._parenthesis_first = token
            if token in self.BRACKETS:
                brackets.append(self.BRACKETS[token])
            elif token == brackets[-1]:
                brackets.pop()
                if not brackets:
                    self._expect_type = False
                    self._closed_type = (token != ')'
                                         or not self._reference_parenthesis)
                    self._qualified_type = False
                    self._function_arrow = (token == ')'
                                            and not self._reference_parenthesis
                                            and self._parenthesis_first != '(')
            return
        if self._predicate_subject is not False and self._consume_predicate_prefix(token, newline):
            return
        if newline and self.ends_before_line_token(token):
            self.saved_token = token
            self.statemachine_return()
            return
        if (self._qualified_type or self._assertion_subject) and self._consume_type_identifier(token, newline):
            return
        if token == 'is':
            self._assertion_predicate = False
        if token == 'extends' and not self._qualified_type:
            self._conditional_type = True
        if token == '?' and self._conditional_type:
            self._conditional_type = False
            self._conditional_questions += 1
            self._expect_type = True
        elif token == ':' and self._conditional_questions:
            self._conditional_questions -= 1
            self._expect_type = True
        elif token == '=>' and self._function_arrow:
            self._function_arrow = False
            self._expect_type = True
        elif token == '-' and self._expect_type:
            return
        elif (token in self.END_TOKENS
              or token == '{' and not self._expect_type
              or token == '<' and self._closed_type and not self._expect_type):
            self.saved_token = token
            self.statemachine_return()
        elif token in self.BRACKETS:
            self._reference_parenthesis = (
                token == '(' and not self._expect_type and not self._closed_type)
            brackets.append(self.BRACKETS[token])
            self._parenthesis_first = None if token == '(' else token
            self._function_arrow = False
        else:
            first = token[:1]
            if token == 'asserts' and self._expect_type:
                self._assertion_subject = True
            self._closed_type = not self._qualified_type and (
                token in self.NON_GENERIC_TYPES
                or first in ('\"', "'", '`') or first.isdigit()
                or first == '<' and token.endswith('>'))
            self._expect_type = not self._qualified_type and (
                token in ('|', '&', 'keyof', 'typeof', 'readonly', 'unique', 'infer', 'extends', 'in')
                or token in ('is', 'as') and not self._expect_type)
            self._qualified_type = token == '.'
            self._function_arrow = False

    def _consume_predicate_prefix(self, token, newline):
        # Check contextual type keywords separately. Other parameter names
        # already leave the reader ready for 'is'.
        if self._predicate_subject is None:
            self._predicate_subject = token in ('asserts', 'keyof', 'infer', 'readonly', 'unique')
        else:
            self._predicate_subject = False
            if token == 'is' and not newline:
                self._assertion_subject = False
                self._expect_type = True
                return True
        return False

    def _consume_type_identifier(self, token, newline):
        if self._assertion_subject:
            self._assertion_subject = False
            if not newline and (token == '\\' or token.replace('$', '_').isidentifier()):
                self._assertion_predicate = True
                return True
        if self._qualified_type and token.replace('$', '_').isidentifier():
            self._qualified_type = False
            self._closed_type = False
            self._expect_type = False
            self._function_arrow = False
            return True
        return False

    def _ends_line(self, token):
        if self.runtime_assertion and token in ('as', 'is'):
            return token == 'as' or not self._assertion_predicate
        if self._class_member:
            # A postfix '[' stays on the same line. A wrapped 'extends'
            # can constrain an 'infer' variable in an open conditional type.
            # 'as' can only continue a mapped type inside its brackets.
            if token in ('[', 'as', '\\'):
                return True
            if token == 'extends':
                return not self._conditional_type
            if token == 'is':
                return not self._assertion_predicate
            if not token[0].isascii() and is_identifier(token):
                return True
        return token not in ('extends', 'is', 'in', 'as') and starts_statement(token)

    def has_complete_type(self):
        return not (self._expect_type or self._conditional_type
                    or self._conditional_questions or self._qualified_type)

    def ends_before_line_token(self, token):
        return (not self._brackets and self.has_complete_type()
                and self._ends_line(token))

    def mismatched_closer(self, token):
        return (self._brackets and token in self.CLOSERS
                and token != self._brackets[-1])


class TypeScriptTypeAliasStates(TypeScriptTypeAnnotationStates):
    """An alias consumes function types and stops before runtime calls."""

    def __init__(self, context):
        super().__init__(context)
        self._after_type_parameters = False
        self._import_type = False
        self._after_function_arrow = False
        self._assertion_type = False

    def _state_global(self, token):
        if not self._brackets and not token.isspace():
            if not (self._expect_type or self._qualified_type) and self._starts_runtime_expression(token):
                self.saved_token = token
                self.statemachine_return()
                return
            self._after_type_parameters = self._expect_type and token[0] == '<'
            self._import_type = token == 'import' and not self._qualified_type
            if token in ('as', 'is') and (self._expect_type or self._qualified_type):
                self._expect_type = self._qualified_type = self._closed_type = False
                self._line_break = False
                return
            if self._after_function_arrow:
                self._assertion_type = token == 'asserts'
            self._after_function_arrow = token == '=>' and self._function_arrow
            if (token == 'new' and not self._qualified_type
                    and (self._expect_type or self.last_token == 'abstract')):
                # A constructor type has its own parameters and return arrow.
                self._expect_type = True
                return
            if self._qualified_type:
                self._expect_type = True
        super()._state_global(token)
        if token == 'is':
            self._assertion_type = False

    def ends_before_line_token(self, token):
        if (token == 'as' or token == 'is' and not self._assertion_type):
            return not self._brackets and self.has_complete_type()
        return super().ends_before_line_token(token)

    def _starts_runtime_expression(self, token):
        if token == '(':
            return not (self._after_type_parameters or self._import_type)
        # A line break ends a type before postfix brackets or type arguments.
        newline = self._line_break or self.context is not None and self.context.newline
        return newline and (
            token[0] in '[<\\'
            or token not in ('in', 'instanceof') and not starts_statement(token) and is_identifier(token))


class TypeAliasContext(TypeScriptTypeAliasStates):
    """Recognize an alias in the stream without changing function parsing."""

    def __init__(self):
        super().__init__(None)
        self.in_body = False
        self.next(self._name)

    def _name(self, token):
        if token.isspace():
            if '\n' in token:
                self.statemachine_return()
        elif token == '\\':
            self.next(self._escaped_name)
        elif is_identifier(token):
            self.next(self._name_parts)
        else:
            self.statemachine_return()

    def _after_name(self, token):
        if token == '=':
            self.in_body = True
            self.next(self._state_global)
        elif token == '<':
            self.br_count = 1
            self.next(self._type_parameters)
        elif not token.isspace() and not (
                token[0] == '<' and packed_type_arguments(token)):
            self.statemachine_return()

    def _name_parts(self, token):
        # The lexer can split an identifier at a combining mark, a
        # connector other than underscore, or a zero-width joiner.
        if token == '\\':
            self.next(self._escaped_name)
        elif not is_identifier_part(token):
            self.next(self._after_name)
            self._after_name(token)

    def _escaped_name(self, token):
        # The tokenizer splits identifier escapes, including the braces in
        # a Unicode code point escape, into separate tokens.
        if token[0] in '<=' or token.isspace():
            self.next(self._after_name)
            self._after_name(token)
        elif token not in ('\\', '{', '}') and not is_identifier_part(token):
            self.statemachine_return()

    def _type_parameters(self, token):
        if token == '<':
            self.br_count += 1
        elif token == '>':
            self.br_count -= 1
            if not self.br_count:
                self.next(self._after_name)


class TypeArguments:  # pylint: disable=too-few-public-methods
    """Follow a possible '<...>' while its tokens pass through the reader."""

    def __init__(self):
        self._argument = TypeScriptTypeAnnotationStates(None)
        self._previous = '<'
        self.simple = True

    def __call__(self, token):
        """Return the next state and whether this token closes the list."""
        if token.startswith(('//', '/*')):
            return self, False
        if token in _NON_TYPE_OPERATORS or isinstance(token, JSXMarkup):
            return None, False
        if self.simple:
            self.simple = _simple_type_token(self._previous, token)
        self._previous = token
        argument = self._argument
        if argument is None:
            if token == '>':  # An optional trailing comma.
                return None, True
            argument = self._argument = TypeScriptTypeAnnotationStates(None)
        if argument.mismatched_closer(token):
            return None, False
        if argument(token):
            if token == ',' and argument.has_complete_type():
                self._argument = None
            else:
                return None, token == '>' and argument.has_complete_type()
        return self, False


def _simple_type_token(previous, token):
    """Recognize reference types without ambiguous expression groups."""
    if token == 'super':
        return False
    if previous == '[':
        return token == ']'  # An array suffix, but not a value subscript.
    if token in _TYPE_SEPARATORS or token == '[':
        return True
    if token == '.':
        return (previous.replace('$', '_').isidentifier()
                and previous not in TypeScriptTypeAnnotationStates.NON_GENERIC_TYPES)
    first = token[0]
    if first == '<' and packed_type_arguments(token):
        return True
    if previous == 'typeof':
        return token not in _EXPRESSION_PREFIXES and token.replace('$', '_').isidentifier()
    # TSX keeps a whole template in one token, including any expressions.
    return (previous == '-' and first.isdigit()
            or previous in _TYPE_PREFIXES
            and (first in "'\"-" or first.isdigit()
                 or first == '`' and '${' not in token
                 or is_identifier(token)))
