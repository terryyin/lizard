"""Confirm ambiguous return annotations before analysis consumes their tokens."""

from .javascript_bindings import JavaScriptBindings
from .javascript_expression import ends_expression, starts_statement
from .typescript_type_annotation import TypeScriptTypeAnnotationStates


class ReturnTypeColon(str):
    """A colon confirmed to belong to an arrow's return annotation."""


class _CallTypeArgumentsStart(str):
    """The opening angle of a confirmed call's type arguments."""


CALL_TYPE_ARGUMENTS_START = _CallTypeArgumentsStart('<')


def type_arguments_followed_by_call(tokens):
    """Confirm an angle group without passing a property boundary."""
    annotation = TypeScriptTypeAnnotationStates(None)
    consumed_type = False
    for token in tokens:
        if token.isspace() or token.startswith(('//', '/*')):
            continue
        if not annotation(token):
            consumed_type = True
            continue
        if not consumed_type:
            return False
        if token == '>':
            return next((item for item in tokens if not item.isspace()
                         and not item.startswith(('//', '/*'))), None) == '('
        if token != ',':
            return False
        annotation = TypeScriptTypeAnnotationStates(None)
        consumed_type = False
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
