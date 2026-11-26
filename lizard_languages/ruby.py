'''
Language parser for Ruby
'''

import re

from .rubylike import RubylikeReader, RubylikeStateMachine, _ends_value
from .js_style_regex_expression import js_style_regex_expression
from .ruby_documentation import may_have_documentation, pattern_for_fragment
from .source_documentation import tokens_ignoring_documentation
from .token_pattern import compiled_token_pattern, tokens_in_span


class MyToken(str):
    def __new__(cls, value, *_):
        val = value.group(0) if hasattr(value, 'group') else value
        return super(MyToken, cls).__new__(cls, val)

    def __init__(self, value):
        super(MyToken, self).__init__()
        self.begin = value.start() if hasattr(value, 'start') else 0


class _HeredocCall(MyToken):
    def __init__(self, match, executable):
        super().__init__(match)
        self.opener = MyToken(match['heredoc_opener'])
        self.opener.begin = match.start('heredoc_opener')
        start, end = match.span('heredoc_suffix')
        self.suffix_start = start - self.begin
        self.suffix_end = end - self.begin
        self.body_start = match.start('heredoc_body') - self.begin - 1 if executable else None
        self.command = match['heredoc_command']


class _HeredocLines(str):
    """Line breaks in a literal whose opener was already emitted."""


class _HeredocQueue(MyToken):
    def __init__(self, match):
        super().__init__(match)
        self.openers = match['heredoc_queue']
        self.command = match['heredoc_queue_command']


def _heredoc_token(match):
    if match['heredoc_queue']:
        return _HeredocQueue(match)
    body = match['heredoc_body']
    executable = body and '#{' in body and match['heredoc_quote'] != "'" and any(
        marker[0] == '#{' and not _HEREDOC_REFERENCE.match(body, marker.end())
        for marker in _INTERPOLATION_MARKERS.finditer(body))
    if match['heredoc_command'] or match['heredoc_suffix'] or executable:
        return _HeredocCall(match, executable)
    return MyToken(match)


class _MethodName(str):
    """A keyword that names a method, `m.end(0)` or `def self.for(user)`:
    neither the reader nor a metric reads it as a structure."""

    is_member_name = True


# The words that the reader or a metric reads as a keyword.
_KEYWORDS = frozenset((
    'begin', 'end', 'do', 'then', 'def', 'class', 'module', 'it', 'not', 'in',
    'if', 'unless', 'else', 'elsif', 'elseif', 'elif', 'case', 'when',
    'while', 'until', 'for', 'foreach', 'and', 'or', 'rescue', 'ensure',
    'try', 'catch', 'finally', 'except', 'switch', 'with', 'return',
    'yield'))
_NAME_BEFORE = frozenset(('.', 'def'))
_BRANCH_BOUNDARIES = frozenset(('then', 'elsif', 'else', 'when', 'in', 'ensure'))


def _paired(opening, closing):
    '''A % literal from its opening to its closing bracket. A pair of
    the same brackets nests, as in `%w(a (b) c)` and `%r{#{a}}`. Real code
    nests at most two levels deep; this allows three.'''
    opening, closing = re.escape(opening), re.escape(closing)
    body = r"(?:\\.|[^" + opening + closing + r"\\])*"
    for _ in range(3):
        body = (r"(?:\\.|[^" + opening + closing + r"\\]|" +
                opening + body + closing + r")*")
    return opening + body + closing


def _any_delimited(one_line, name, excluded=''):
    '''A bracket pair, or any other punctuation that closes itself, such
    as `%q@a@` or `%q#a#`, except the `excluded`. Such a delimiter ends
    the literal on its line, so a `%` in a regex that the reader misses,
    such as `/^%w/`, cannot run far. Only a `%r` regex with such a
    delimiter can span lines, as an `x` flag regex often does.'''
    pairs = "|".join(_paired(pair[0], pair[1])
                     for pair in ('{}', '()', '[]', '<>'))
    line_end = r"\n" if one_line else ""
    delimiter = "(?P=" + name + ")"
    other = ("(?P<" + name + r">[^\w\s(){}\[\]<>" + excluded + "])" +
             r"(?:\\.|(?!" + delimiter + r")[^\\" + line_end + "])*" +
             delimiter)
    return "(?:" + pairs + "|" + other + ")"


# A `%` right after an operand, as in `n%i==0`, is modulo. So is a bare
# `%=`, as in `a %= 2`.
_PERCENT_LITERAL = (r"(?<![\w)\]}])\%(?:[qQwWiIsx]" +
                    _any_delimited(True, 'percent_typed') +
                    r"|" + _any_delimited(True, 'percent_bare', '=') +
                    r"|r" + _any_delimited(False, 'percent_regex') +
                    r"[a-z]*)")


# The opener can stand alone, follow an assignment, or appear in a call.
# Keep a shift such as pages <<~mask separate.
_HEREDOC_MEMBER = r"\.[A-Za-z_]\w*[!?]?(?:\([ \t]*(?:'[^'\r\n]*'|:\w+|\w+)?[ \t]*\))?"
_HEREDOC_COMMAND_NAME = r"[a-z_]\w*[!?]?"
_HEREDOC_COMMAND_PREFIX = r"^[ \t]*(?P<heredoc_command>" + _HEREDOC_COMMAND_NAME + r")[ \t]+"
_MAYBE_HEREDOC_COMMAND = re.compile(_HEREDOC_COMMAND_PREFIX + r"<<", re.M)
_SIMPLE_HEREDOC = (
    r"(?:(?<!\\\n)(?<!\\\r\n)^[ \t]*|(?<==)[ \t]*|(?P<heredoc_call>(?<=[(,])[ \t]*)|"
    + _HEREDOC_COMMAND_PREFIX + r")"
    r"(?P<heredoc_opener><<(?P<heredoc_indent>[-~])?"
    r"(?P<heredoc_quote>['\"`])?(?P<heredoc_label>[A-Za-z_]\w*)"
    r"(?(heredoc_quote)(?P=heredoc_quote)))"
    r"(?P<heredoc_suffix>(?:" + _HEREDOC_MEMBER + r")*"
    r"(?(heredoc_call)(?:[ \t]*,[ \t]*(?:\w+:[ \t]+)?[:@]?\w+)*[ \t]*[)\]]*)"
    r"(?:[ \t]*\#[^\r\n]*)?)[ \t]*\r?\n"
    r"(?P<heredoc_body>(?:(?!^(?(heredoc_indent)[ \t]*)(?P=heredoc_label)\r?$)[^\n]*\n)*)"
    r"^(?(heredoc_indent)[ \t]*)(?P=heredoc_label)\r?$"
)
_QUEUE_LABEL = r"""(?:'[A-Za-z_]\w*'|"[A-Za-z_]\w*"|`[A-Za-z_]\w*`|[A-Za-z_]\w*)"""
_QUEUE_OPENER = r"<<[-~]?" + _QUEUE_LABEL
_QUEUE_ARGUMENT = r"(?:\w+:[ \t]+)?" + _QUEUE_OPENER
_QUEUE_LINE = (
    r"(?:(?<=[(\[,])[ \t]*|^[ \t]*(?P<heredoc_queue_command>" + _HEREDOC_COMMAND_NAME
    + r")[ \t]+)(?P<heredoc_queue>" + _QUEUE_ARGUMENT
    + r"(?:[ \t]*,[ \t]*" + _QUEUE_ARGUMENT + r")+)"
    r"[ \t]*[)\]]*(?:[ \t]+(?:if|unless)\b[^#\r\n]+)?[ \t]*(?:\#[^\r\n]*)?\r?\n"
)
_QUEUE_OPENER_RE = re.compile(r"<<([-~]?)(" + _QUEUE_LABEL + ")")
_HEREDOC_REFERENCE = re.compile(r"(?:[A-Z]\w*(?:::[A-Z]\w*)*|[@$]?[a-z_]\w*)\}")


_INTERPOLATION_MARKERS = re.compile(r"\\.|#\{", re.S)


class RubyStateMachine(RubylikeStateMachine):
    structure_depth = None
    ns_structures = frozenset(('if', 'for', 'while', 'do'))
    modifier_structure = False
    structure_peak = 0
    statement_structure_peak = 0

    def _state_global(self, token):
        if self.structure_depth is not None and (
                token == ';' or (
                    token in _BRANCH_BOUNDARIES and not getattr(token, 'is_member_name', False))
                or (self.is_newline() and self.last_token is not None
                    and _ends_value(self.last_token) and token not in ('.', '&', ')', ']', '}'))):
            self.statement_structure_peak = 0
        super()._state_global(token)

    def _start_structure(self, opener, callback, token, function_body):
        child = self.statemachine_clone()
        depth = 0 if function_body else self.structure_depth
        child.structure_depth = depth + (opener in self.ns_structures)
        if opener == 'do' and self._state == self._modifier_do:
            child.structure_depth += self.modifier_structure
        child._record_structure(child.structure_depth)

        def finish_block():
            if not function_body:
                self.statement_structure_peak = max(self.statement_structure_peak, child.structure_peak)
                self.structure_peak = max(self.structure_peak, child.structure_peak)
            if callback is not None:
                callback()

        self.sub_state(child, finish_block, token)

    def _count_loop_modifier(self, token):
        if self.structure_depth is not None and token in self.ns_structures:
            self._record_structure(self._modified_statement_depth() + 1)

    def _modified_statement_depth(self):
        return max(self.structure_depth, self.statement_structure_peak)

    def _record_structure(self, depth):
        self.structure_peak = max(self.structure_peak, depth)
        function = self.context.current_function
        function.max_nested_structures = max(function.max_nested_structures, depth)

    _loop_depth = False
    _loop_keyword = None

    def _start_block(self, opener, callback=None, token=None, function_body=False):
        if opener != 'def' and getattr(self.context, 'ruby_tracks_depth', False):
            callback = self._depth_block_callback(opener, callback)
        if self.structure_depth is None:
            super()._start_block(opener, callback, token, function_body)
        else:
            structure = self._loop_keyword if opener == 'loop' else opener
            self._start_structure(structure, callback, token, function_body)

    def _depth_block_callback(self, opener, callback):
        function = self.context.current_function
        owns_depth = self._loop_depth if opener == 'loop' else (
            opener in ('if', 'case') and self._take_pending_depth())
        entry_depth = function.nesting_depth - owns_depth
        parent_peak = function.ruby_block_peak
        statement_peak = function.ruby_statement_peak
        function.ruby_block_peak = function.nesting_depth
        function.ruby_statement_peak = 0

        def close_depth():
            completed_peak = function.ruby_block_peak
            function.ruby_block_peak = (
                None if parent_peak is None else max(parent_peak, completed_peak))
            function.ruby_statement_peak = statement_peak
            if completed_peak > entry_depth:
                function.ruby_statement_peak = max(statement_peak, completed_peak)
            if owns_depth:
                self.context.add_nd_condition(-1)
            if callback is not None:
                callback()

        return close_depth

    def _start_loop(self, opener, state):
        self._loop_keyword = opener
        self._loop_depth = (opener in ('while', 'for')
                            and getattr(self.context, 'ruby_tracks_depth', False)
                            and self._take_pending_depth())
        super()._start_loop(opener, state)

    def _take_pending_depth(self):
        # Keep a keyword's level before a brace or line break can close it.
        function = self.context.current_function
        if not function.levels_awaiting_body:
            return False
        function.levels_awaiting_body -= 1
        return True

    def _loop_modifier(self, token):
        if token == 'while':
            self._record_modified_block_depth()
        self._count_loop_modifier(token)

    def _read_modifier(self, token):
        self.modifier_structure = token in self.ns_structures
        if self.structure_depth is not None and self.modifier_structure:
            self._record_structure(self._modified_statement_depth() + 1)
        if token == 'if':
            self._record_modified_block_depth()
        super()._read_modifier(token)

    def _record_modified_block_depth(self):
        if getattr(self.context, 'ruby_tracks_depth', False):
            function = self.context.current_function
            if function.ruby_statement_peak:
                function.ruby_statement_peak += 1
                self.context.record_nd_peak(function.ruby_statement_peak)

    def _if(self, token):
        # A modifier predicate can open a block: `return if jobs.any? do`.
        if token == 'do' and not self.br_count and not getattr(token, 'is_member_name', False):
            self.next(self._modifier_do)
        else:
            if (token in ('if', 'while') and self.follows_value() and not self.br_count
                    and not getattr(token, 'is_member_name', False)
                    and getattr(self.context, 'ruby_tracks_depth', False)
                    and not self.context.get_condition_depth()):
                self._record_modified_block_depth()
            super()._if(token)

    def _finish_modifier_do(self):
        self.next(self._state_global)

    def _modifier_do(self, token):
        # A block with parameters or a body on the next line has a
        # distinct opening. Keep reading the predicate on other tokens.
        if self.is_newline() or token in ('|', '||', ';'):
            self._start_block('do', callback=self._finish_modifier_do, token=token)
        else:
            self.next(self._if, token)


class RubyReader(RubylikeReader):
    # pylint: disable=R0903

    ext = ['rb']
    language_names = ['ruby']
    STATE_MACHINE = RubyStateMachine
    _control_flow_keywords = RubylikeReader._control_flow_keywords | {'unless'}

    def track_nested_structures(self):
        self.parallel_states[0].structure_depth = 0

    def nesting_semicolons(self, tokens):
        self.context.ruby_tracks_depth = True
        self.block_starts = ('{', '${')
        boundaries = _NestingSemicolons(self.context)
        return boundaries.read(tokens), boundaries

    def preprocess(self, tokens):
        """Drop spaces, and mark a keyword right after '.', '&.' or 'def'
        on the same line as the name of a method.  At the end of a line,
        call `context.at_line_end` once if the state machine set it."""
        keywords, name_before = _KEYWORDS, _NAME_BEFORE
        context = self.context
        context.at_line_end = None
        if getattr(context, 'ruby_tracks_depth', False):
            tokens = _mark_logical_assignments(tokens)
        name_follows = False
        tokens = _expand_percent_interpolation(tokens)
        for token in tokens:
            if type(token) is _HeredocLines:
                # The opener is the literal's token. Its disjoint body
                # contributes physical lines after the call closes.
                for _ in token:
                    yield '\n'
                context.add_nloc(len(token))
                continue
            if token.isspace():
                if token != '\n':
                    continue
                if context.at_line_end is not None:
                    # The state machine has read the last token of the
                    # line.  The next line decides if the expression ends.
                    yield token
                    token = yield from self._after_line_end(tokens)
                    if token is None:
                        return
            if name_follows and token in keywords:
                name_follows = False
                yield _MethodName(token)
                continue
            name_follows = token in name_before
            yield token
        if context.at_line_end is not None:
            context.at_line_end()

    def _after_line_end(self, tokens):
        """Find the next token after blank and comment lines.  Unless it
        goes on with the expression, `.map` or `&.map`, call
        `context.at_line_end` first.  Then pass on the skipped lines, so
        that a comment such as `#lizard forgive` comes after the end."""
        context = self.context
        skipped = []
        token = None
        for token in tokens:
            if token.isspace() and token != '\n':
                continue
            if token == '\n' or self.get_comment_from_token(token) is not None:
                skipped.append(token)
                continue
            break
        else:
            token = None
        if token not in ('.', '&'):
            at_line_end, context.at_line_end = context.at_line_end, None
            at_line_end()
        yield from skipped
        return token

    @staticmethod
    @js_style_regex_expression(language='ruby')
    def generate_tokens(source_code, addition='', token_class=None):
        addition = (r"|\w+:|\$\w+|\.+|:?\@{0,2}\w+\??\!?"
                    r"|" + _PERCENT_LITERAL + addition)
        plain_tokens = addition
        read_token = MyToken
        if "<<" in source_code:
            addition += "|" + _QUEUE_LINE + "|" + _SIMPLE_HEREDOC
            read_token = _heredoc_token
        percent_strings = '%(' in source_code or '%Q(' in source_code
        has_documentation = may_have_documentation(source_code)
        has_heredoc_commands = _MAYBE_HEREDOC_COMMAND.search(source_code) is not None
        bracket_stack = []
        source = source_code
        position = 0
        known_locals = set()
        while source is not None:
            tokens = tokens_ignoring_documentation(
                source, addition, read_token, pattern_for_fragment(source, source is source_code),
                has_documentation, start=position)
            position = 0
            if has_heredoc_commands:
                tokens = _keep_local_shifts(tokens, source, plain_tokens, known_locals)
            for token in tokens:
                if type(token) is _HeredocQueue:
                    bodies = _queued_heredoc_bodies(token, source)
                    header = compiled_token_pattern(r'|\#[^\n]*' + plain_tokens + '|' + _QUEUE_OPENER)
                    end_of_opener = token.begin + len(token)
                    yield from tokens_in_span(header, source, token.begin,
                                              end_of_opener - bool(bodies), MyToken)
                    if bodies:
                        for start, position, quoted in bodies:
                            body = ([] if quoted else
                                    _interpolation_tokens(source, start, position, plain_tokens))
                            yield from body or (_HeredocLines('\n' * source.count('\n', start, position)),)
                        break
                    continue
                if token == "{":
                    bracket_stack.append("{")
                elif token == "}":
                    if bracket_stack:
                        if bracket_stack.pop() == '#{':
                            source = '"'+source[token.begin + 1:]
                            yield token
                            break
                elif token.startswith('"'):
                    first, sep, _ = token.partition('#{')
                    if sep:
                        yield first + '"'
                        yield '${'  # because #will be regarded as comments
                        bracket_stack.append(sep)
                        source = source[token.begin + token.find(sep)+2:]
                        break
                if type(token) is _HeredocCall:
                    if token.command is not None:
                        yield token.command
                    yield token.opener
                    yield from tokens_in_span(
                        compiled_token_pattern(r'|\#[^\n]*' + addition), token,
                        token.suffix_start, token.suffix_end, MyToken)
                    body = (_interpolation_tokens(token, token.body_start, len(token), addition)
                            if token.body_start is not None else [])
                    yield from body or (_HeredocLines('\n' * token.count('\n')),)
                else:
                    if percent_strings and token.startswith(('%(', '%Q(')) and '#{' in token:
                        token.percent_interpolation = addition
                    yield token
            else:
                source = None


def _queued_heredoc_bodies(token, source):
    bodies = []
    start = token.begin + len(token) - 1
    for indentation, label in _QUEUE_OPENER_RE.findall(token.openers):
        quoted = label.startswith("'")
        prefix = r'[ \t]*' if indentation else ''
        delimiter = re.compile(r'^' + prefix + re.escape(label.strip("'\"`")) + r'\r?$', re.M)
        closing = delimiter.search(source, start + 1)
        if closing is None:
            return []
        bodies.append((start, closing.end(), quoted))
        start = closing.end()
    return bodies


def _keep_local_shifts(tokens, source, addition, known_locals):
    """A receiver already declared as a local uses << as an operator."""
    line_names = set()
    header = None
    previous = None
    pattern_depth = 0
    pattern_key = None
    expression_depth = 0
    statement_tokens = 0
    pin_depth = 0
    pin_scopes = []
    pattern = compiled_token_pattern(r'|\#[^\n]*' + addition)
    for candidate in tokens:
        if type(candidate) in (_HeredocCall, _HeredocQueue) and candidate.command in known_locals:
            parts = tokens_in_span(pattern, source, candidate.begin,
                                   candidate.begin + len(candidate), MyToken)
        else:
            parts = (candidate,)
        for token in parts:
            opens_pin = header == 'pattern' and token == '(' and previous == '^'
            if header == 'pattern' and not opens_pin and not token.startswith('#'):
                ends_key = token in (',', '}', ']', ')', 'if', 'unless', 'then', ';') or (
                    token == '\n' and not pattern_depth)
                if ends_key and pattern_key is not None:
                    known_locals.add(pattern_key)
                if ends_key or not token.isspace():
                    pattern_key = None
                if token in ('{', '[', '('):
                    pattern_depth += 1
                elif token in ('}', ']', ')') and pattern_depth:
                    pattern_depth -= 1
                elif pin_depth and token == ')':
                    header = None
                elif token in ('if', 'unless', 'then'):
                    header = None
            if opens_pin:
                # Treat the pin as an expression that can contain its own pattern.
                pin_scopes.append((pin_depth, pattern_depth))
                pin_depth = 1
                pattern_depth = 0
                header = None
            elif pin_depth == 1 and token == ')':
                pin_depth, pattern_depth = pin_scopes.pop()
                header = 'pattern'
            elif pin_depth and token == '(':
                pin_depth += 1
            elif pin_depth and token == ')':
                pin_depth -= 1
            if token in ('\n', ';'):
                if header != 'pattern' or not pattern_depth:
                    header = None
                line_names.clear()
                if token == ';' or (previous is not None and (
                        _ends_value(previous) or previous in ('begin', 'do', 'else', 'ensure', 'then'))):
                    statement_tokens = 0
            elif token in ('=', '+=', '-=', '*=', '/=', '%=', '<<='):
                known_locals.update(line_names)
            elif token == 'def' and previous != '.':
                known_locals.clear()
                header = token
            elif token == 'for':
                header = token
            elif token == 'in' and previous != '.':
                header = None if header == 'for' else 'pattern'
                pattern_depth = 0
            elif (token == '=>' and header is None and not expression_depth and
                  statement_tokens == 1 and previous.lstrip('@$').rstrip('?!').isidentifier()):
                header = 'pattern'
                pattern_depth = 0
            elif token == '|' and header != 'pattern':
                header = 'block' if previous in ('do', '{') else None
            elif token.isidentifier():
                line_names.add(token)
                if header == 'def':
                    header = 'parameters'
                elif header:
                    known_locals.add(token)
            elif header == 'pattern' and token.endswith(':') and token[:-1].isidentifier():
                pattern_key = token[:-1]
            if token in ('{', '[', '('):
                expression_depth += 1
            elif token in ('}', ']', ')') and expression_depth:
                expression_depth -= 1
            if not token.isspace() and not token.startswith('#'):
                previous = token
                if token != ';':
                    statement_tokens += 1
            yield token


def _expand_percent_interpolation(tokens):
    for token in tokens:
        addition = getattr(token, 'percent_interpolation', None)
        if addition is None:
            yield token
        else:
            yield from _percent_interpolation_tokens(token, addition)


def _percent_interpolation_tokens(token, addition):
    start = token.index('(') + 1
    return _interpolation_tokens(token, start, len(token) - 1, addition) or [token]


def _interpolation_tokens(token, start, end, addition):
    fragments = []
    pattern = compiled_token_pattern(r'|\#[^\n]*' + addition)

    while start < end:
        marker = next((match for match in _INTERPOLATION_MARKERS.finditer(token, start, end)
                       if match[0] == '#{'), None)
        if marker is None:
            break
        expression, closing = _interpolation_expression(token, marker.end(), end, pattern)
        if closing is None:
            return []
        fragments.extend(('"'+token[start:marker.start()]+'"', '${'))
        fragments.extend(expression)
        fragments.append(closing)
        start = closing.begin + 1

    if not fragments:
        return []
    fragments.append('"'+token[start:end]+'"')
    return fragments


def _interpolation_expression(token, start, end, pattern):
    parts = []
    braces = 0
    for current in tokens_in_span(pattern, token, start, end, MyToken):
        if current == '{':
            braces += 1
        elif current == '}':
            if not braces:
                return parts, current
            braces -= 1
        parts.append(current)
    return parts, None


class _NestingSemicolons:
    """A Ruby statement normally ends at its line break."""

    def __init__(self, context):
        self.context = context
        self.before_token = False

    def read(self, tokens):
        previous = None
        for token in tokens:
            if previous == 'else':
                self.context.clear_prev_was_else()
            self.before_token = (self.context.newline and previous is not None
                                 and token not in ('.', '&')
                                 and (previous in ('do', 'then') or _ends_value(previous)))
            if (token == ';' or (self.before_token and token not in (')', ']', '}'))
                    or (token in _BRANCH_BOUNDARIES and not getattr(token, 'is_member_name', False))):
                self.context.current_function.ruby_statement_peak = 0
            previous = token
            yield token


def _mark_logical_assignments(tokens):
    for token in tokens:
        if token in ('&&', '||'):
            following = next(tokens, None)
            if following == '=' and token.begin + len(token) == following.begin:
                token.is_logical_assignment = True
            yield token
            if following is not None:
                yield following
        else:
            yield token
