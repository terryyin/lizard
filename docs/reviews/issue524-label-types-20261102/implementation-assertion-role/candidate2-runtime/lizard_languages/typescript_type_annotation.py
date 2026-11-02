"""Consume type-owned brackets and return enclosing runtime delimiters."""

from .code_reader import CodeStateMachine
from .javascript_functions import starts_statement


class TypeScriptTypeAnnotationStates(CodeStateMachine):
    BRACKETS = {'(': ')', '[': ']', '{': '}', '<': '>', '${': '}'}
    END_TOKENS = frozenset(('=', ';', ')', ']', '}', ',', '=>',
                            '||', '&&', '??', '?', ':', '+', '-', '*', '/',
                            '%', '**', '==', '===', '!=', '!=='))

    def __init__(self, context):
        super().__init__(context)
        self.saved_token = None
        self._brackets = []
        self._expect_type = True
        self._function_arrow = False
        self._parenthesis_first = None
        self._conditional_type = False
        self._conditional_questions = 0
        self._line_break = False

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
                    self._function_arrow = (token == ')'
                                            and self._parenthesis_first != '(')
            return
        if (newline and not self._expect_type
                and token not in ('extends', 'is', 'in', 'as')
                and starts_statement(token)):
            self.saved_token = token
            self.statemachine_return()
            return
        if token == 'extends':
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
              or token == '{' and not self._expect_type):
            self.saved_token = token
            self.statemachine_return()
        elif token in self.BRACKETS:
            brackets.append(self.BRACKETS[token])
            self._parenthesis_first = None if token == '(' else token
            self._function_arrow = False
        else:
            self._expect_type = token in (
                '|', '&', 'keyof', 'typeof', 'readonly', 'unique', 'infer',
                'extends', 'is', 'in', 'as')
            self._function_arrow = False
