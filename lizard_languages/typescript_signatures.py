"""Confirm ambiguous return annotations before analysis consumes their tokens."""

from .javascript_bindings import JavaScriptBindings
from .javascript_expression import ends_expression, starts_statement
from .typescript_type_annotation import TypeScriptTypeAnnotationStates


class CallTypeArgument(str):
    """An erased token in a confirmed call's type arguments."""

    is_type_argument = True


def call_type_argument_token_count(tokens):
    """Count confirmed type arguments up to their closing angle."""
    annotation = TypeScriptTypeAnnotationStates(None)
    consumed_type = False
    has_separator = False
    count = 0
    for token in tokens:
        if token.isspace():
            continue
        count += 1
        if token.startswith(('//', '/*')):
            continue
        if not annotation(token):
            consumed_type = True
            continue
        if not consumed_type:
            return 0
        if token == '>' and has_separator:
            following = next((part for part in tokens if not part.isspace()
                              and not part.startswith(('//', '/*'))), None)
            return count if following == '(' else 0
        if token != ',':
            return 0
        has_separator = True
        annotation = TypeScriptTypeAnnotationStates(None)
        consumed_type = False
    return 0


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
