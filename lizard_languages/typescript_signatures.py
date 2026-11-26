"""Confirm ambiguous return annotations before analysis consumes their tokens."""

from .javascript_bindings import JavaScriptBindings
from .javascript_expression import ends_expression, starts_statement
from .javascript_templates import (
    NESTED_SUBSTITUTION_END, NESTED_SUBSTITUTION_START, NESTED_TEMPLATE_END,
    NESTED_TEMPLATE_START, TemplateBoundary, interpolation_code)
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


def template_call_type_arguments(tokens, tokenize, optional=False, template_checks=None):
    """Confirm template literal types in calls and instantiation expressions."""
    if template_checks is None:
        template_checks = {}
    annotation = _TemplateType()
    saw_template = in_interpolation = False
    count = 0
    for token in tokens:
        if token.isspace():
            continue
        count += 1
        if token.startswith(('//', '/*')):
            continue
        if in_interpolation or token.startswith('`'):
            is_type = _source_template_type(token, tokenize, in_interpolation, template_checks)
            if is_type is False:
                return 0
            saw_template |= is_type is True
            in_interpolation = False
        elif token == '${':
            saw_template = in_interpolation = True
        if annotation(token):
            if not annotation.complete():
                return 0
            if token == ',':
                annotation = _TemplateType()
            elif token == '>' and saw_template:
                return count if _follows_type_arguments(tokens, optional) else 0
            else:
                return 0
    return 0


def _source_template_type(token, tokenize, in_expression, checks):
    # Lookahead replays the same token until the main reader reaches it.
    key = id(token)
    cached = checks.get(key)
    if cached is not None and cached[1] == in_expression:
        return cached[2]
    is_type = None
    if in_expression or '${' in token:
        is_type = _template_is_type(token, tokenize, in_expression)
    # Keep the token alive while its identity is in the cache.
    checks[key] = (token, in_expression, is_type)
    return is_type


def erase_template_types(tokens):
    """Keep runtime groups and remove complete type arguments in an interpolation."""
    tokens = list(tokens)
    groups = _template_groups(tokens)
    index = 0
    previous = ''
    while index < len(tokens):
        token = tokens[index]
        group = groups.get(index)
        if (token == '<' and group and group.valid and group.has_template
                and (previous == '?.' or ends_expression(previous))
                and _follows_type_arguments(
                    _tokens_from(tokens, group.end + 1), previous == '?.')):
            index = group.end + 1
            continue
        index += 1
        if token is NESTED_TEMPLATE_END:
            continue
        if not _type_whitespace(token):
            previous = token
        yield str(token) if isinstance(token, TemplateBoundary) else token


def _tokens_from(tokens, start):
    for index in range(start, len(tokens)):
        yield tokens[index]


class _TypeGroup:
    __slots__ = ('end', 'valid', 'has_template')

    def __init__(self, end, valid=False, has_template=False):
        self.end = end
        self.valid = valid
        self.has_template = has_template


def _template_groups(tokens):
    """Summarize closed groups from the inside out, visiting each child once."""
    groups = {}
    openers = []
    for index, token in enumerate(tokens):
        closer = _group_closer(token)
        if closer is not None:
            openers.append((index, closer))
            continue
        if (token in (')', ']', '}') or token is NESTED_TEMPLATE_END):
            # An unmatched comparison cannot extend across a closing group.
            while openers and openers[-1][1] == '>':
                openers.pop()
        if openers and _closes_group(token, openers[-1][1]):
            start, _ = openers.pop()
            groups[start] = _summarize_group(tokens, start, index, groups)
    return groups


def _group_closer(token):
    if token is NESTED_TEMPLATE_START:
        return NESTED_TEMPLATE_END
    if token is NESTED_SUBSTITUTION_START:
        return NESTED_SUBSTITUTION_END
    return TypeScriptTypeAnnotationStates.BRACKETS.get(token)


def _closes_group(token, closer):
    if isinstance(token, TemplateBoundary) or isinstance(closer, TemplateBoundary):
        return token is closer
    return token == closer


def _summarize_group(tokens, start, end, groups):
    opening = tokens[start]
    is_template = opening is NESTED_TEMPLATE_START
    brackets = not isinstance(opening, TemplateBoundary) and opening != '<'
    annotation = _TemplateType()
    if brackets:
        annotation(opening)
    has_template = is_template
    index = start + 1
    while index < end:
        child = groups.get(index)
        if child:
            if not child.valid:
                return _TypeGroup(end)
            has_template |= child.has_template
            if not is_template and any(annotation(part) for part in _group_tokens(tokens, index, child.end)):
                return _TypeGroup(end)
            index = child.end + 1
            continue
        token = tokens[index]
        index += 1
        if _type_whitespace(token):
            continue
        if is_template:
            return _TypeGroup(end)
        if annotation(token):
            if opening != '<' or token != ',' or not annotation.complete():
                return _TypeGroup(end)
            annotation = _TemplateType()
    valid = is_template or (not (brackets and annotation(tokens[end])) and annotation.complete())
    return _TypeGroup(end, valid, has_template)


def _group_tokens(tokens, start, end):
    opening = tokens[start]
    if opening is NESTED_TEMPLATE_START:
        yield "''"
        return
    yield opening
    if opening == '(':
        # The type reader distinguishes a grouped type from parameter names.
        for index in range(start + 1, end):
            token = tokens[index]
            if not _type_whitespace(token):
                if token == '(':
                    yield '('
                    yield ')'
                break
    yield tokens[end]


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


def _template_is_type(source, tokenize, in_expression):
    tokens = [NESTED_TEMPLATE_START]
    for part in interpolation_code(source, in_expression, delimiters=True, track_nested=True):
        if isinstance(part, str):
            tokens.append(part)
        else:
            start, end = part
            tokens.extend(tokenize(source[start:end]))
    tokens.append(NESTED_TEMPLATE_END)
    group = _template_groups(tokens).get(0)
    return group is not None and group.valid


class _TemplateType(TypeScriptTypeAnnotationStates):
    """Type lookahead must keep runtime decisions in the token stream."""

    _CLASS_MEMBER_FOLLOWERS = (':', '?', '(', '<')

    def __init__(self):
        super().__init__(None)
        self._conditional_depths = set()
        self._class_parameter_depths = set()
        self._expected = ()

    def complete(self):
        return not (self._expected or self._brackets or self._expect_type
                    or self._conditional_type or self._conditional_questions
                    or self._qualified_type)

    def _state_global(self, token):
        depth = len(self._brackets)
        expected = self._expected
        self._expected = ()
        if expected is self._CLASS_MEMBER_FOLLOWERS and token.startswith('<'):
            if token == '<':
                self._class_parameter_depths.add(depth)
            elif token.endswith('>'):
                self._expected = ('(',)
            else:
                self.statemachine_return()
                return
        elif token in ('&&', '||', '??') or (expected and token not in expected):
            self._expected = expected
            self.statemachine_return()
            return
        if token == 'class' and self.last_token not in ('.', '?.'):
            self._expected = self._CLASS_MEMBER_FOLLOWERS
        elif token == 'extends' and (self.last_token == '>'
                                     or ends_expression(self.last_token or '')):
            self._conditional_depths.add(depth)
        elif token == '?':
            if depth not in self._conditional_depths:
                self._expected = (':', ',', ')', ']')
            self._conditional_depths.discard(depth)
        super()._state_global(token)
        after = len(self._brackets)
        if after < depth:
            self._conditional_depths.discard(depth)
            if after in self._class_parameter_depths:
                self._class_parameter_depths.discard(after)
                # Generic methods must have parameters. Classes can have heritage.
                self._expected = ('(',)


def _type_whitespace(token):
    return token.isspace() or token.startswith(('//', '/*'))
