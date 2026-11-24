"""Consume type-owned brackets and return enclosing runtime delimiters."""

from .code_reader import CodeStateMachine
from .javascript_expression import starts_statement


class TypeScriptTypeAnnotationStates(CodeStateMachine):
    NON_GENERIC_TYPES = frozenset((
        'any', 'unknown', 'never', 'number', 'bigint', 'boolean', 'string',
        'symbol', 'object', 'void', 'undefined', 'null', 'this', 'true', 'false'))
    BRACKETS = {'(': ')', '[': ']', '{': '}', '<': '>', '${': '}'}
    END_TOKENS = frozenset(('=', ';', ')', ']', '}', ',', '=>',
                            '||', '&&', '??', '?', ':', '+', '-', '*', '/',
                            '%', '**', '==', '===', '!=', '!==', '>', '>=', '<=',
                            'instanceof', 'in', '^'))

    def __init__(self, context, return_type=False, class_member=False):
        super().__init__(context)
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
        if newline and not (self._expect_type or self._qualified_type) and self._ends_line(token):
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
        return token not in ('extends', 'is', 'in', 'as') and starts_statement(token)
