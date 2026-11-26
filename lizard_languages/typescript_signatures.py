"""Confirm ambiguous return annotations before analysis consumes their tokens."""

from .javascript_bindings import JavaScriptBindings
from .javascript_expression import ends_expression, starts_statement
from .javascript_templates import (
    NESTED_SUBSTITUTION_END, NESTED_SUBSTITUTION_START, NESTED_TEMPLATE_END,
    NESTED_TEMPLATE_START, interpolation_code)
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


def template_call_type_arguments(tokens, tokenize, optional=False):
    """Confirm template literal types in calls and instantiation expressions."""
    annotation = TypeScriptTypeAnnotationStates(None)
    saw_template = in_interpolation = False
    count = 0
    for token in tokens:
        if token is NESTED_TEMPLATE_END or token is NESTED_SUBSTITUTION_END:
            return 0
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
        elif token is NESTED_TEMPLATE_START:
            size = _nested_template_size(tokens)
            if size is None:
                return 0
            count += size
            saw_template = True
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
                return count if _follows_type_arguments(tokens, optional) else 0
            else:
                return 0
    return 0


def _nested_template_size(tokens):
    """Validate each substitution and count the flattened template tokens."""
    depth = 1
    substitutions = []
    count = 0
    for token in tokens:
        if token is NESTED_TEMPLATE_END:
            depth -= 1
            if not depth:
                return count
            continue
        if token.isspace():
            continue
        count += 1
        if token is NESTED_SUBSTITUTION_START:
            substitutions.append(TypeScriptTypeAnnotationStates(None))
        elif token is NESTED_SUBSTITUTION_END:
            if not _complete_template_type(substitutions.pop()):
                return None
        elif not _type_whitespace(token):
            if substitutions[-1](token):
                return None
            if token is NESTED_TEMPLATE_START:
                depth += 1
    return None


def _follows_type_arguments(tokens, optional):
    newline = False
    for token in tokens:
        if _type_whitespace(token):
            newline |= '\n' in token
            continue
        if token == '(':
            return True
        if optional or token in ('<', '>', '+', '-'):
            return False
        return (newline or token is NESTED_TEMPLATE_START or token.startswith('`')
                or token in (')', ']', '}', ',', ';', ':', '?', '.', '?.',
                             '=', '&&', '||', '??', '==', '===', '!=', '!==',
                             '|', '&', '^', '*', '/', '%', '**', 'as', 'satisfies',
                             'in', 'instanceof'))
    return not optional


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
