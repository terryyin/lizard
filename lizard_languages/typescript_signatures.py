"""Confirm ambiguous return annotations before analysis consumes their tokens."""

from itertools import chain

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


class GenericArrowTypeOpen(str):
    """An operand-position type parameter list containing a function type."""


def generic_arrow_has_function_constraint(tokens):
    code = (token for token in tokens
            if not token.isspace() and not token.startswith(('//', '/*')))
    parameter = next(code, '')
    if parameter == 'const':
        parameter = next(code, '')
    follower = next(code, None)
    if not parameter.replace('$', '_').isidentifier() or follower not in ('extends', '=', ','):
        return False

    openers = TypeScriptTypeAnnotationStates.BRACKETS
    closers = ['>']
    contains_arrow = False
    for token in chain((follower,), code):
        if token == '=>':
            contains_arrow = True
        elif token in openers:
            closers.append(openers[token])
        elif token == closers[-1]:
            closers.pop()
            if not closers:
                return contains_arrow and next(code, None) == '('
        elif token in ('>', ')', ']', '}') or token == ';' and closers[-1] != '}':
            return False
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
