"""Confirm ambiguous return annotations before analysis consumes their tokens."""

from .javascript_bindings import JavaScriptBindings
from .javascript_expression import ends_expression, starts_statement
from .typescript_type_annotation import TypeScriptTypeAnnotationStates


class ReturnTypeColon(str):
    """A colon confirmed to belong to an arrow's return annotation."""


def return_type_colon(tokens, depth):
    """Use the existing type reader; a conditional must keep its separator."""
    annotation = TypeScriptTypeAnnotationStates(None)
    for token in tokens:
        if token.isspace() or token.startswith(('//', '/*')):
            continue
        if annotation(token):
            token = annotation.saved_token
            if token != '=>':
                return False
            return not depth or _has_branch_separators(tokens, depth)
    return False


def _has_branch_separators(tokens, required):
    closing = JavaScriptBindings.BRACKETS
    stack = []
    questions = 0
    previous = ''
    line_start = False
    for token in tokens:
        if token.isspace() or token.startswith(('//', '/*')):
            line_start |= '\n' in token
            continue
        if not stack:
            if (line_start and ends_expression(previous)
                    and starts_statement(token)):
                return False
            if token in (';', ',', ')', ']', '}'):
                return False
            if token == '?':
                questions += 1
            elif token == ':':
                if not questions:
                    required -= 1
                    if not required:
                        return True
                else:
                    questions -= 1
        if token in closing:
            stack.append(closing[token])
        elif stack and token == stack[-1]:
            stack.pop()
        previous = token
        line_start = False
    return False


class TypeArgumentComma(str):
    """A comma within a TypeScript call's type arguments."""

    is_type_argument_comma = True


def call_type_argument_span(tokens):
    """Return the span of comma-delimited reference types before a call."""
    depth = 1
    expects_type = True
    previous = '<'
    comma = False
    for offset, token in enumerate(tokens, 1):
        if token.isspace() or token.startswith(('//', '/*')):
            continue
        if previous == '[':
            if token != ']':
                return 0
        elif expects_type:
            if token in ('keyof', 'readonly', 'typeof'):
                continue
            if token == '>' and previous == ',':
                depth -= 1
                expects_type = False
            elif token.replace('$', '_').isidentifier() or token[0] in "'\"":
                expects_type = False
            else:
                return 0
        elif token == '<':
            depth += 1
            expects_type = True
        elif token == '>':
            depth -= 1
        elif token in (',', '|', '&', '.'):
            comma |= token == ','
            expects_type = True
        elif token != '[' and not _packed_type_reference(token):
            return 0
        if not depth:
            return offset if comma and _next_code_token(tokens) == '(' else 0
        previous = token
    return 0


def _packed_type_reference(token):
    # The TSX lexer emits a simple '<Name>' as one token.
    return token[0] == '<' and token.endswith('>') and (
        token[1:-1].replace('.', '_').isidentifier()
        and not getattr(token, 'is_jsx_markup', False))


def _next_code_token(tokens):
    return next((token for token in tokens if not (
        token.isspace() or token.startswith(('//', '/*')))), None)
