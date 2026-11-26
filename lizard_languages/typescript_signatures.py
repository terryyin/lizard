"""Confirm ambiguous return annotations before analysis consumes their tokens."""

from .javascript_bindings import JavaScriptBindings
from .javascript_expression import ends_expression, starts_statement
from .javascript_templates import interpolation_code
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


def template_call_type_arguments(tokens, tokenize):
    """Confirm template literal types before the opening '(' of a call."""
    annotation = TypeScriptTypeAnnotationStates(None)
    saw_template = in_interpolation = False
    count = 0
    for token in tokens:
        if token.isspace():
            continue
        count += 1
        if token.startswith(('//', '/*')):
            continue
        if in_interpolation:
            if not _interpolation_is_type(token, tokenize):
                return 0
            in_interpolation = False
        elif token == '${':
            saw_template = in_interpolation = True
        elif token.startswith('`') and '${' in token:
            if not all(_interpolation_is_type(token[start:end], tokenize)
                       for start, end in interpolation_code(token, False)):
                return 0
            saw_template = True
        if annotation(token):
            if not _complete_template_type(annotation):
                return 0
            if token == ',':
                annotation = TypeScriptTypeAnnotationStates(None)
            elif token == '>' and saw_template:
                following = next((part for part in tokens if not _type_whitespace(part)), None)
                return count if following == '(' else 0
            else:
                return 0
    return 0


def _interpolation_is_type(source, tokenize):
    annotation = TypeScriptTypeAnnotationStates(None)
    for token in tokenize(source):
        if not _type_whitespace(token) and annotation(token):
            return False
    return _complete_template_type(annotation)


def _complete_template_type(annotation):
    return not (annotation._brackets or annotation._expect_type
                or annotation._conditional_type or annotation._conditional_questions
                or annotation._qualified_type)


def _type_whitespace(token):
    return token.isspace() or token.startswith(('//', '/*'))
