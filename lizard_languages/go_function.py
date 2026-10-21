"""Bounded lookahead for Go signatures, without consuming analysis tokens.

Looking ahead in the source keeps line and complexity counters on the original
token stream. In particular, a function type never opens a candidate function
or absorbs a following statement while waiting for a body.
"""

import re


_DECLARATION_NAME = re.compile(r'\s*(?!func\b)[^\W\d]\w*')
_CLOSING_BRACKET = {'(': ')', '[': ']', '{': '}'}


def starts_declaration(source, position):
    return _DECLARATION_NAME.match(source, position) is not None


class GoToken(str):
    def __new__(cls, value, signature):
        token = str.__new__(cls, value)
        token.signature = signature
        return token


def function_signature(matches):
    tokens = _SignatureTokens(matches)
    first = tokens.peek(newlines=True)
    if first != '(':
        return ('declaration' if first.isidentifier() and first != 'func'
                else 'type'), 0, 0
    tokens.balanced()
    parameters = tokens.count
    tokens.read_type()
    following = tokens.peek()
    if following == '(':
        # The first parentheses were a method receiver, followed by its name.
        return 'declaration', 0, 0
    return ('literal' if following == '{' else 'type'), \
        tokens.count, parameters


def variable_declaration(matches):
    tokens = _SignatureTokens(matches)
    name = tokens.peek()
    if not name.isidentifier():
        return '', 0
    tokens.take()
    tokens.read_type()
    if tokens.peek() == '=':
        tokens.take()
        return name, tokens.count
    return '', tokens.count


class _SignatureTokens:
    def __init__(self, matches):
        self.matches = matches
        self.pending = None
        self.count = 0

    def peek(self, newlines=False):
        while self.pending is None:
            match = next(self.matches, None)
            token = match.group(0) if match else ''
            if token.startswith(('//', '/*')):
                if '\n' in token:
                    token = '\n'
                else:
                    continue
            if token.isspace() and (token != '\n' or newlines):
                continue
            self.pending = token
        if newlines and self.pending == '\n':
            self.pending = None
            return self.peek(newlines=True)
        return self.pending

    def take(self, newlines=False):
        token = self.peek(newlines)
        self.pending = None
        if token and token != '\n':
            self.count += 1
        return token

    def balanced(self):
        closing = _CLOSING_BRACKET
        stack = [closing[self.take(newlines=True)]]
        for match in self.matches:
            token = match.group(0)
            if token.isspace() or token.startswith(('//', '/*')):
                continue
            self.count += 1
            if token in closing:
                stack.append(closing[token])
            elif token == stack[-1]:
                stack.pop()
                if not stack:
                    return

    def read_type(self, newlines=False):
        token = self.peek(newlines)
        if token in ('*', '**', '<', 'chan'):
            self.take()
            if self.peek(newlines=True) == '<':
                self.take()
            if self.peek(newlines=True) == '-':
                self.take()
            self.read_type(newlines=True)
        elif token in ('(', '['):
            self.balanced()
            if token == '[':
                self.read_type()
        elif token == 'map':
            self.take()
            if self.peek() == '[':
                self.balanced()
                self.read_type()
        elif token in ('struct', 'interface'):
            self.take()
            if self.peek(newlines=True) == '{':
                self.balanced()
        elif token == 'func':
            self.take()
            if self.peek(newlines=True) == '(':
                self.balanced()
                self.read_type()
        elif token.isidentifier() and token not in (
                'const', 'var', 'type', 'return', 'if', 'for', 'switch'):
            self.take()
            if self.peek() == '.':
                self.take()
                self.take()
            if self.peek() == '[':
                self.balanced()
