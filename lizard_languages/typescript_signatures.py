"""Confirm ambiguous return annotations before analysis consumes their tokens."""

from .javascript_bindings import JavaScriptBindings
from .javascript_expression import ends_expression, starts_statement
from .typescript_type_annotation import TypeScriptTypeAnnotationStates


class ReturnTypeColon(str):
    """A colon confirmed to belong to an arrow's return annotation."""


class ConstructorTypeOpen(str):
    """The start of a constructor type argument list with a function type."""


def constructor_has_function_type(tokens):
    annotation = TypeScriptTypeAnnotationStates(None)
    function_type = False
    previous = None
    bare_void = False
    for token in tokens:
        if token.isspace() or token.startswith(('//', '/*')):
            continue
        # A nested arrow with a bare void result cannot be a value
        # expression. The runtime void operator requires an operand.
        if bare_void and token in (')', ']', '}', ',', ';', '>'):
            function_type = True
        if token == '=>' and annotation.accepts_function_type_arrow():
            function_type = True
        bare_void = previous == '=>' and token == 'void'
        previous = token
        if annotation(token):
            if token != ',':
                return token == '>' and function_type
            annotation = TypeScriptTypeAnnotationStates(None)
    return False


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
