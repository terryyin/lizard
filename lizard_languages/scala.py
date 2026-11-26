'''
Language parser for Scala
'''
import re

from .code_reader import CodeReader
from .clike import CCppCommentsMixin
from .golike import GoLikeStates
from .token_pattern import compiled_token_pattern, tokens_in_span
__author__ = 'David Baum'

# A line break inside a body does not end it before a token that cannot
# start a statement, or after an operator that needs a right operand.
_CONTINUES_FROM_LINE_START = frozenset((
    '.', '{', 'else', 'catch', 'finally', 'match', 'with', 'yield'))
_CONTINUES_AT_LINE_END = frozenset((
    '&&', '||', '+', '-', '*', '/', '%', '++', '::', '==', '!=', '=>',
    '<=', '>=', '<', '|', '&', '^', '->', '=', '+=', '-=', ':',
    ',', '.', 'else', 'then', 'do', 'yield', 'try', 'finally', 'match',
    'catch', 'new', 'throw'))
_CONDITION_KEYWORDS = frozenset(('if', 'while', 'for'))
_SIGNATURE_CONTINUES = frozenset((
    '(', '[', '{', '=', ':', '.', '@', 'with', 'match', 'case', '=>', '&', '|', '<', '>', '#'))
_GROUP_STARTS = frozenset(('(', '[', '{'))
_GROUP_ENDS = frozenset((')', ']', '}'))
_DECLARATION_ENDS = frozenset((';', '}'))
_DECLARATION_STARTS = frozenset(('def', 'val', 'var', 'class', 'object', 'trait', 'import', 'package'))
_TYPE_NAME = re.compile(r'[\w$]+|`[^`]+`')
_TYPE_OPERATORS = frozenset(('with', 'forSome', 'match', 'case'))


_XML_NAME = r'[^\W\d][\w.:-]*'
_XML_START = re.compile(r'(?<![^ \t\r\n({>])<(?=[^\W\d]|[!?])')
_TAG_PART = re.compile(r"""(?:"[^"]*"|'[^']*')|/>|[>{]""")
_TAG_XML_NAME = re.compile(r'</?' + _XML_NAME)
_CONTENT_PART = re.compile(r'<!--.*?-->|<!\[CDATA\[.*?\]\]>|<\?.*?\?>|</?' + _XML_NAME + r'|\{\{|\{', re.S)


_STRING_OPENERS = frozenset(('"', '"""'))
_LOCATED_DELIMITERS = _STRING_OPENERS | {'}', 'def', '$'}
_STRING_DELIMITERS = r'|"""|(?<=[\w$`])"'
_SCALA_QUOTES = r"|'[^\W\d][\w$]*(?![\w$'])|'(?=[{\[][^'])"
_PROJECTED_MACRO = re.compile(r'#\s*[\w$]+\s*=\s*\$\{')
_SYMBOLIC_TYPES = r'|=:=|<:<|=>>|\?=>'
_SYMBOLIC_METHOD = re.compile(r'([ \t]+)((?:[^\W\d][\w$]*_)?(?:(?!/[*/])[!#%&*+\-/:<=>?@\\^|~])+)')
_SYMBOLIC_DEF = re.compile(r'\bdef' + _SYMBOLIC_METHOD.pattern)
_COMMENT_BRACKETS = re.compile(r'/\*|\*/')
_RAW_END = re.compile(r'"""+')
_RAW_INTERPOLATION = re.compile(r'\$\$|\$"|\$\{|"""+')
_INTERPOLATION = re.compile(r'\\[\\"]|\$\$|\$"|\$\{|"|\n')


class _InterpolationBrace(str):
    is_scala_interpolation = True


class _MultilineStringPart(str):
    is_scala_string_part = True


class _XmlText(str):
    @property
    def is_scala_string_part(self):
        return '\n' in self


class _XmlCaseBoundary(str):
    ends_scala_case = True


class _XmlCaseGuard(str):
    is_scala_case_guard = True


_INTERPOLATION_OPEN = _InterpolationBrace('{')
_INTERPOLATION_CLOSE = _InterpolationBrace('}')


def _string_part(text):
    return _MultilineStringPart(text) if '\n' in text else text


class _LocatedToken(str):
    def __new__(cls, text, offset):
        token = super().__new__(cls, text)
        token.offset = offset
        return token


def _locate_delimiter(match):
    text = match.group()
    if (text in _LOCATED_DELIMITERS or (text.startswith('/*') and '/*' in text[2:])
            or (text == '<' and _XML_START.match(match.string, match.start()))):
        return _LocatedToken(text, match.start())
    return text


def _scala_tokens(patterns, source, pos=0, in_interpolation=False, macro_splices=False):
    """Read code until the end of the file or the matching interpolation brace."""
    depth = 0
    end = len(source)
    xml_enabled = True
    pattern = patterns[in_interpolation or macro_splices]
    while pos < end:
        for token in tokens_in_span(pattern, source, pos, end, _locate_delimiter,
                                    macros=False):
            if xml_enabled and token == '<' and isinstance(token, _LocatedToken):
                xml_end, parts = _complete_xml(patterns, source, token.offset)
                if xml_end is not None:
                    yield from parts
                    pos = xml_end
                    break
                xml_enabled = False
            if token in _STRING_OPENERS:
                pos = yield from _string_tokens(patterns, source, token)
                break
            if isinstance(token, _LocatedToken) and token.startswith('/*'):
                pos = _nested_comment_end(source, token.offset)
                yield source[token.offset:pos]
                break
            if macro_splices and token == '$' and source.startswith('{', token.offset + 1):
                # Read a Scala 3 splice as a balanced block. Quotes inside it
                # can name values and do not always start character literals.
                yield '$'
                yield '{'
                pos = yield from _scala_tokens(patterns, source, token.offset + 2, True, macro_splices)
                yield '}'
                break
            if token == 'def':
                name = _SYMBOLIC_METHOD.match(source, token.offset + 3)
                if name:
                    yield 'def'
                    yield name.group(1)
                    yield name.group(2)
                    pos = name.end()
                    break
            if in_interpolation:
                if token == '{':
                    depth += 1
                elif token == '}':
                    if not depth:
                        return token.offset + 1
                    depth -= 1
            yield token
        else:
            return end
    return end


def _complete_xml(patterns, source, start):
    # The compiler also prints pseudo tags in generated output. Keep the
    # original tokens if the apparent XML cannot close.
    stream = _xml_tokens(patterns, source, start)
    parts = []
    try:
        while True:
            parts.append(next(stream))
    except StopIteration as end:
        return end.value, parts


def _xml_tokens(patterns, source, start):
    """Read XML text and hand embedded blocks back to the Scala scanner."""
    tags = []
    tag = None
    closing = False
    segment = pos = start
    while True:
        pattern = _TAG_PART if tag is not None else _CONTENT_PART
        match = pattern.search(source, pos)
        if match is None or (pos == start and match.start() != start):
            return None
        part = match.group()
        pos = match.end()
        if part == '{':
            yield _XmlText('"' + source[segment:match.start()] + '"')
            pos = segment = yield from _interpolation_tokens(patterns, source, pos)
        elif tag is not None:
            if part in ('>', '/>'):
                if closing:
                    if not tags or tags.pop() != tag or part != '>':
                        return None
                elif part == '>':
                    # A generated pseudo tag can have the same opening
                    # shape. Reject it before scanning any Scala splices.
                    if not tags and source.find('</' + tag, pos) < 0:
                        return None
                    tags.append(tag)
                tag = None
                if not tags:
                    break
        elif _TAG_XML_NAME.fullmatch(part):
            closing = part.startswith('</')
            tag = part[2:] if closing else part[1:]
        elif not tags:
            break
    if pos > segment:
        yield _XmlText('"' + source[segment:pos] + '"')
    return pos


class _IndentedMatch:
    def __init__(self, indent, brace_depth):
        self.indent = indent
        self.brace_depth = brace_depth
        self.has_xml = False

    def ends_before(self, token, indent, brace_depth):
        if self.brace_depth != brace_depth:
            return False
        if token == '}':
            return True
        return indent is not None and (indent < self.indent or
                                       (indent == self.indent and token != 'case'))


class _XmlIndentBoundary(str):
    def __new__(cls, token, opening, activating, closing):
        boundary = super().__new__(cls, token)
        boundary.token = token
        boundary.xml_indent_events = opening, activating, closing
        return boundary

    def __getattr__(self, name):
        return getattr(self.token, name)


def _tokens_with_indentation(tokens):
    leading = ''
    for token in tokens:
        if token == '\n':
            leading = ''
        elif leading is not None:
            if token.isspace():
                leading += token
            elif not token.startswith(('//', '/*')):
                yield token, len(leading.expandtabs(8))
                leading = None
                continue
        yield token, None


def _xml_case_tokens(tokens):
    """Mark sibling case boundaries in brace and indentation matches."""
    tokens = _tokens_with_indentation(tokens)
    braces = []
    indents = []
    after_match = after_line_start_end = match_end_marker = False
    for token, indent in tokens:
        closing = ()
        while indents and indents[-1].ends_before(token, indent, len(braces)):
            closing += (indents.pop().has_xml,)
        opening = after_match and not match_end_marker and token == 'case' and indent is not None
        if opening:
            indents.append(_IndentedMatch(indent, len(braces)))

        braced_case = bool(braces and braces[-1] is not None)
        indented_case = (indents and indents[-1].brace_depth == len(braces)
                         and indent == indents[-1].indent)
        if token == 'case' and (braced_case or indented_case):
            activating = False
            head = _case_head(tokens)
            if head and head[-1] in ('if', '=>'):
                has_xml = any(isinstance(part, _XmlText) for part in head)
                if indented_case:
                    activating = has_xml and not indents[-1].has_xml
                    indents[-1].has_xml |= has_xml
                    has_xml = indents[-1].has_xml
                else:
                    braces[-1] |= has_xml
                    has_xml = braces[-1]
                if has_xml:
                    token = _XmlCaseBoundary(token)
                    if head[-1] == 'if':
                        head[-1] = _XmlCaseGuard(head[-1])
            if opening or activating or closing:
                token = _XmlIndentBoundary(token, opening, activating, closing)
            yield token
            yield from head
            after_match = after_line_start_end = match_end_marker = False
            continue

        if token == '{':
            braces.append(False if after_match else None)
        elif token == '}' and braces:
            if braces.pop():
                token = _XmlCaseBoundary(token)
        if not (token.isspace() or token.startswith(('//', '/*'))):
            after_match = token == 'match'
            # In a standalone `end match`, the next case can belong to an outer match.
            match_end_marker = after_match and after_line_start_end and indent is None
            after_line_start_end = token == 'end' and indent is not None
        yield _XmlIndentBoundary(token, False, False, closing) if closing else token


def _case_head(tokens):
    """Read only the pattern and its first separator."""
    head = []
    depth = 0
    for token, _ in tokens:
        head.append(token)
        if not depth and token in ('if', '=>', 'class', 'object', ';'):
            break
        if token in ('(', '[', '{'):
            depth += 1
        elif token in (')', ']', '}'):
            depth -= 1
            if depth < 0:
                break
    return head


def _nested_comment_end(source, start):
    depth = 1
    for marker in _COMMENT_BRACKETS.finditer(source, start + 2):
        depth += 1 if marker.group() == '/*' else -1
        if not depth:
            return marker.end()
    return len(source)


def _interpolation_tokens(patterns, source, start):
    yield _INTERPOLATION_OPEN
    end = yield from _scala_tokens(patterns, source, start, True)
    yield _INTERPOLATION_CLOSE
    return end


def _string_tokens(patterns, source, opener):
    """Yield literal text and read each interpolation as a block of Scala code."""
    start = opener.offset
    pos = start + len(opener)
    interpolated = start > 0 and (
        source[start - 1].isalnum() or source[start - 1] in '_$`')
    if opener == '"""':
        special = _RAW_INTERPOLATION if interpolated else _RAW_END
    else:
        special = _INTERPOLATION
    segment = start
    while True:
        match = special.search(source, pos)
        if match is None:
            end = len(source)
            yield _string_part('"' + source[segment:end] + '"')
            return end
        marker = match.group()
        pos = match.end()
        if marker == '${':
            prefix = '' if segment == start else '"'
            yield _string_part(prefix + source[segment:match.start()] + '$')
            pos = segment = yield from _interpolation_tokens(patterns, source, pos)
        elif marker[0] == '"' or marker == '\n':
            end = match.start() if marker == '\n' else pos
            prefix = '' if segment == start else '"'
            yield _string_part(prefix + source[segment:end])
            return end


class ScalaReader(CodeReader, CCppCommentsMixin):
    # pylint: disable=R0903

    ext = ['scala']
    language_names = ['scala']
    has_scala_interpolation = True

    # Separated condition categories
    _control_flow_keywords = {'if', 'for', 'while', 'catch', 'do'}
    _logical_operators = {'&&', '||'}
    _case_keywords = {'case'}  # Pattern matching
    _ternary_operators = {'?'}

    def __init__(self, context):
        super(ScalaReader, self).__init__(context)
        self.parallel_states = [ScalaStates(context)]

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        addition = r'|`[^`\r\n]+`' + _SYMBOLIC_TYPES + addition
        if token_class is None and (
                '"""' in source_code or '${' in source_code or '$"' in source_code
                or _SYMBOLIC_DEF.search(source_code) or _XML_START.search(source_code)):
            addition = _STRING_DELIMITERS + addition
            patterns = (compiled_token_pattern(addition), compiled_token_pattern(_SCALA_QUOTES + addition))
            tokens = _scala_tokens(patterns, source_code, macro_splices=bool(_PROJECTED_MACRO.search(source_code)))
            if 'case' in source_code and _XML_START.search(source_code):
                return _xml_case_tokens(tokens)
            return tokens
        pattern = compiled_token_pattern(addition)
        return tokens_in_span(pattern, source_code, 0, len(source_code), token_class, macros=False)

    def preprocess(self, tokens):
        """Drop spaces and close definitions before the next token counts.
        A '\n' is consumed by line_counter before the states see it."""
        open_bodies = self.parallel_states[0].open_bodies
        # 0: inside a line, 1: at a line start, 2: at a line start after a
        # blank line, 3: after a comment at a line start, which is no blank.
        after_line_break = (1, 2, 2, 1)
        line_start = 0
        for token in tokens:
            if not token.isspace():
                if line_start:
                    if not token.startswith(('//', '/*')):
                        if open_bodies:
                            open_bodies[-1].end_definition_before(
                                token, line_start == 2)
                        line_start = 0
                    elif line_start == 1:
                        line_start = 3
                elif token.startswith('/*') and '\n' in token:
                    # A block comment that ends on a later line.
                    line_start = 3
                elif (token in _DECLARATION_ENDS or token in _DECLARATION_STARTS) and open_bodies:
                    open_bodies[-1].end_signature_before(token, False)
                if token == '?' and isinstance(token, _XmlIndentBoundary):
                    # Keep the reader's operator marker on a dedent token.
                    token.token = getattr(self, '_question_mark', token.token)
                yield token
            elif token == '\n':
                line_start = after_line_break[line_start]
                yield token


class ScalaStates(GoLikeStates):  # pylint: disable=R0903

    FUNC_KEYWORD = 'def'
    TYPE_KEYWORD = None

    def __init__(self, context, open_bodies=None):
        super().__init__(context)
        self._brackets = 0
        self._type_operand = False
        self._type_infix = False
        self._contextual_clause = False
        self._contextual_parameter_named = False
        # (bracket depth, keyword) of each open `if (`, `while (`, `for (`.
        self._conditions = []
        # The keyword whose condition the last ')' closed, or None.
        self._after_condition = None
        self._open_dos = 0
        # The token before the last one in the body: `counter.+` selects a
        # member named `+`, which needs no right operand.
        self._previous = None
        # All nested states share the stack of open signatures and expression bodies.
        self.open_bodies = [] if open_bodies is None else open_bodies

    def statemachine_clone(self):
        return ScalaStates(self.context, self.open_bodies)

    def _function_name(self, token):
        if token.startswith('`') and len(token) > 2:
            self.context.add_to_function_name(token[1:-1])
            self._state = self._expect_function_dec
        else:
            super()._function_name(token)

    def _expect_function_dec(self, token):
        if token == '(':
            self._brackets = 1
            self.open_bodies.append(self)
            self._state = self._first_parameter_in_clause
        elif token == '[':
            super()._expect_function_dec(token)
        else:
            # A definition without parameters, such as `def size = 1`, is
            # not a function. Its tokens count in the enclosing function.
            self._fold_declaration()

    def _function_parameters(self, token):
        if self._contextual_clause and self._brackets == 1:
            if token == ':':
                self._contextual_parameter_named = True
            elif token in (',', ')'):
                self._finish_contextual_parameter()
        if token in _GROUP_STARTS:
            self._brackets += 1
        elif token in _GROUP_ENDS:
            self._brackets -= 1
        if not self._brackets:
            self._state = self._after_parameter_list
        elif token == ',' and self._brackets > 1:
            # Keep commas in a tuple, type application or default expression
            # with their parameter.
            self.context.current_function.full_parameters[-1] += ' ,'
            self.context.add_to_long_function_name(' ,')
        elif token not in ('(', ')'):
            self.context.parameter(token)

    def _after_parameter_list(self, token):
        if token == '(':
            self._brackets = 1
            self._state = self._first_parameter_in_clause
        else:
            self.next(self._expect_function_impl, token)

    def _first_parameter_in_clause(self, token):
        self._contextual_clause = token == 'using'
        self._contextual_parameter_named = False
        params = self.context.current_function.full_parameters
        if token != ')' and params and params[-1]:
            self.context.parameter(',')
        self.next(self._function_parameters, token)

    def _finish_contextual_parameter(self):
        parameters = self.context.current_function.full_parameters
        if parameters and parameters[-1] and not self._contextual_parameter_named:
            # FunctionInfo counts this slot without adding a binding name.
            parameters[-1] = '()'
        self._contextual_parameter_named = False

    def _expect_function_impl(self, token):
        if not self._brackets:
            if token == ':':
                self._type_operand = self._type_infix = False
                self._state = self._read_result_type
                return
            if token == '{':
                # Scala 2 also allows a procedure body without '='.
                self.open_bodies.pop()
                self.next(self._function_impl, token)
                return
        self._read_result_type(token)

    def _read_result_type(self, token):
        if token in _GROUP_STARTS:
            self._brackets += 1
        elif token in _GROUP_ENDS and self._brackets:
            self._brackets -= 1
        elif token == '=' and not self._brackets:
            self._state = self._expect_function_body
        if not self._brackets:
            is_name = token not in _TYPE_OPERATORS and bool(_TYPE_NAME.fullmatch(token))
            self._type_infix = self._type_operand and is_name
            self._type_operand = token in _GROUP_ENDS or (is_name and not self._type_infix)

    def _expect_function_body(self, token):
        """The body starts after '=', on the same line or the next."""
        self._brackets = 0
        self._conditions = []
        self._open_dos = 0
        self.next(self._function_body, token)

    def _function_body(self, token):
        self._previous = self.last_token
        if token == '{':
            # A line break inside the braces cannot end this body, so the
            # body leaves the open list until its braces close.
            self.open_bodies.pop()
            self.sub_state(self.statemachine_clone(), self._reopen_body)
        elif token in ('(', '['):
            self._open_bracket()
        elif token in (')', ']'):
            self._close_bracket()
        elif token == 'do':
            # Not the `do` of Scala 3 `while (cond) do` or `for (...) do`.
            if not (self.last_token == ')' and
                    self._after_condition in ('while', 'for')):
                self._open_dos += 1
        elif token == '}':
            # The brace closes the enclosing block, which ends on this line
            # and owns the brace.
            self.context.current_function.token_count -= 1
            self._end_function_body()
            self.context.current_function.token_count += 1
            self.context.current_function.end_line = self.context.current_line
            self.next(self._state_global, token)
        elif token == ';' and self._brackets <= 0 and not self._open_dos:
            self._end_function_body()
        elif token == 'def' and self.last_token[0] not in '"`':
            # No expression goes on into a definition. After a quote, the
            # word is text in a multi-line string, which the tokenizer
            # splits, or a name in backticks.
            self._end_function_body()
            self.next(self._state_global, token)

    def _open_bracket(self):
        if self.last_token in _CONDITION_KEYWORDS:
            if self.last_token == 'while' and self._open_dos:
                # The `while` that ends `do ... while (cond)`.
                self._open_dos -= 1
            else:
                self._conditions.append((self._brackets, self.last_token))
        self._brackets += 1

    def _close_bracket(self):
        self._brackets -= 1
        self._after_condition = None
        if self._conditions and self._brackets == self._conditions[-1][0]:
            self._after_condition = self._conditions.pop()[1]

    def _reopen_body(self):
        self.open_bodies.append(self)
        # `for { ... }` is a loop header, like `for (...)`.
        self._after_condition = 'for' if self._previous == 'for' else None

    def end_definition_before(self, token, after_blank_line):
        """A line break can end a signature or expression outside brackets."""
        if self._state in (self._function_body,):
            if self._brackets <= 0 and not self._continues_on(token, after_blank_line):
                self._end_function_body()
        else:
            self.end_signature_before(token, True)

    def end_signature_before(self, token, at_line_start):
        """Fold a declaration whose signature ends without an implementation."""
        if self._brackets or self._state not in (
                self._after_parameter_list, self._expect_function_impl, self._read_result_type):
            return
        if token in _DECLARATION_ENDS or token in _DECLARATION_STARTS or (
                at_line_start and token not in _SIGNATURE_CONTINUES
                and self.last_token not in _CONTINUES_AT_LINE_END
                and not (self._type_infix and _TYPE_NAME.fullmatch(token))):
            self.open_bodies.pop()
            self._fold_declaration()

    def _fold_declaration(self):
        self.fold_into_enclosing_function()
        # The enclosing function already counted the 'def' and its line.
        self.context.current_function.nloc -= 1
        self.context.current_function.token_count -= 1
        self._state = self._state_global

    def _continues_on(self, token, after_blank_line):
        """Whether `token`, the first on a new line, continues the body."""
        last = self.last_token
        if token in _CONTINUES_FROM_LINE_START:
            # Only one line break may come before a `{` argument, and a
            # `{` after a block starts a block of its own.
            return not (token == '{' and (after_blank_line or last == '}'))
        if last in _CONTINUES_AT_LINE_END and self._previous != '.':
            return True
        if last == '>' and self._previous == '>':
            # A shift. One `>` can end an XML literal.
            return True
        if last in (')', '}') and self._after_condition:
            # After `if (cond)` or `for { ... }`, the body comes next.
            return True
        # The `while (cond)` of `do ... while (cond)`.
        return token == 'while' and self._open_dos > 0

    def _end_function_body(self):
        self.open_bodies.pop()
        self.context.end_of_function()
        self._state = self._state_global

    def statemachine_before_return(self):
        if self._state in (self._expect_function_body, self._function_body):
            self.context.end_of_function()
