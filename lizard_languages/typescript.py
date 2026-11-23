'''
Language parser for TypeScript
'''

from itertools import tee

from .code_reader import CodeReader, CodeStateMachine, MemberName
from .clike import CCppCommentsMixin
from .js_style_regex_expression import js_style_regex_expression
from .javascript_bindings import JavaScriptBindings
from .typescript_type_annotation import TypeScriptTypeAnnotationStates
from .typescript_states import TypeScriptStates
from .typescript_signatures import ReturnTypeColon, return_type_colon
from .javascript_templates import LINE_COMMENT, interpolation_code, template_tokens
from .javascript_expression import awaiting_function_body, end_before_line_token
from .javascript_statements import ImpliedSemicolons

# One token, so each ? in ?? is not its own ternary decision.
NULLISH_COALESCING = r"|(?:\?\?)"
OPTIONAL_ACCESS = r"|(?:\?\.(?!\d))"
# An optional member or parameter, "name?: T", "'a-b'?: T" or "f(a?)"; in
# "x?y:0" the "?" opens a ternary.
OPTIONAL_NAME = (r"|(?:(?:\w+|\"(?:\\.|[^\"\\])*\"|\'(?:\\.|[^\'\\])*\')"
                 r"\?(?=\s*(?:[:,)]|\Z)))")
# JavaScript identifiers can contain dollar signs as well as word characters.
JS_IDENTIFIER = r"|(?:[\w$]+)"


class Tokenizer(object):
    def __init__(self):
        self.sub_tokenizer = None
        self._ended = False
        self._aborted = False

    def __call__(self, token):
        if self.sub_tokenizer:
            yield from self._sub_tokens(token)
            if self.sub_tokenizer._ended:
                self.sub_tokenizer = None
            return
        for tok in self.process_token(token):
            yield tok

    def _sub_tokens(self, token):
        yield from self.sub_tokenizer(token)

    def stop(self):
        self._ended = True

    def process_token(self, token):
        pass


class JSTokenizer(Tokenizer):
    def __init__(self):
        super().__init__()
        self.depth = 1

    def _sub_tokens(self, token):
        for tok in self.sub_tokenizer(token):
            if self.sub_tokenizer._aborted:
                # An aborted JSX candidate is ordinary JavaScript. Its
                # emitted braces still belong to this tokenizer.
                yield from JSTokenizer.process_token(self, tok)
            else:
                yield tok

    def process_token(self, token):
        if token == "{":
            self.depth += 1
        elif token == "}":
            self.depth -= 1
            if self.depth == 0:
                self.stop()
                return
        yield token


class _OptionalMemberQuestion(str):
    """A class member marker excluded by optional-aware metric consumers."""

    is_optional = True


_OPTIONAL_MEMBER_QUESTION = _OptionalMemberQuestion('?')
_ANNOTATION_PUNCTUATION = frozenset(('?', ':'))


class _MemberName(str):
    """A keyword that names a member, `promise.catch(f)`, `{ if: a }` or
    `catch(e) {}` in a class, which no metric counts as a structure."""

    is_member_name = True


_MEMBER_ACCESS = ('.', '?.')
_COMMENT_STARTS = ('//', '/*')
# The tokens that preprocess looks at, in one set for the hot loop.
_MARKED_PUNCTUATION = _ANNOTATION_PUNCTUATION | frozenset(_MEMBER_ACCESS)
# With the reader's conditions: the keywords that a metric counts as a
# structure or a jump.
_STRUCTURE_KEYWORDS = frozenset((
    'else', 'do', 'switch', 'try', 'finally', 'break', 'continue', 'with'))


_NAMED_KEYWORDS = frozenset(('function',))
# After a keyword, these show that it names a member: `{ if: a }`,
# `catch = (e) => e`.  The start of a method's parameters, `catch(e)` or
# `catch<T>(e: T)`, shows it only at the start of a member.  TSX reads
# `<T>` as one token.
_NAME_FOLLOWERS = frozenset((':', '='))
_PARAMETER_STARTS = frozenset(('(', '<'))


def _ends_a_number(token):
    """Whether a '.' after this token is a decimal point, `1_000.`."""
    return bool(token) and token[0].isdigit() and token.replace(
        '_', '').isdigit()


class TypeScriptReader(CodeReader, CCppCommentsMixin):
    # pylint: disable=R0903

    ext = ['ts']
    language_names = ['typescript', 'ts']
    # JS/TS split templates into '${' + expression tokens; only TSX/JSX keep
    # atomic templates that need a backtick scan on the fast path.
    _scan_atomic_templates = False
    _class_field_annotations = True

    # Separated condition categories
    _control_flow_keywords = {'if', 'elseif', 'for', 'while', 'catch'}
    _logical_operators = {'&&', '||', '??'}
    _case_keywords = {'case'}
    _ternary_operators = {'?'}

    def __init__(self, context):
        super().__init__(context)
        self.context.class_field_annotations = self._class_field_annotations
        self.parallel_states = [TypeScriptStates(context)]
        self._previous_token = None
        self._implied_semicolons = None

    def implied_semicolons(self, tokens):
        """For the extensions: the tokens, and the finder of the places
        where a statement ends without a ';'.  The first extension that
        asks starts the finder, and the next ones read the same finder.
        An extension asks when it gets the tokens, before it reads one,
        and reads the tokens that it gets back."""
        if self._implied_semicolons is None:
            self._implied_semicolons = ImpliedSemicolons()
            tokens = self._implied_semicolons.read(tokens, self.context)
        return tokens, self._implied_semicolons

    def script_tokens(self, code):
        """Tokens of a piece of code; Vue tokenizes its files as markup."""
        return self.generate_tokens(code)

    def _count_interpolation_conditions(self, token, in_expression):
        # Re-tokenize opaque interpolation spans; keyword members stay non-
        # conditions the same way as on the outer token stream (#546).
        conditions = self.conditions
        earlier = previous = None
        for start, end in interpolation_code(token, in_expression):
            for inner in self.script_tokens(token[start:end]):
                # A word after a dot names a member; `1. ? a : b` is a ternary.
                if (inner in conditions and (previous not in _MEMBER_ACCESS
                                             or not inner[0].isalpha())):
                    self.context.add_condition()
                if (inner == '\n' and previous == '.'
                        and _ends_a_number(earlier)):
                    previous = inner    # the same rule as in preprocess
                elif not (inner.isspace()
                          or inner.startswith(_COMMENT_STARTS)):
                    earlier, previous = previous, inner

    def _count_conditions_from_token(self, previous, token):
        # Split JS/TS exposes '${' then the expression; atomic TSX/JSX
        # templates keep the whole backtick token.
        if previous == '${':
            self._count_interpolation_conditions(token, True)
        elif (type(self)._scan_atomic_templates
                and token[:1] == '`' and '${' in token):
            self._count_interpolation_conditions(token, False)

    def process_token(self, token):
        # Count conditions written inside template interpolations (opaque
        # to the outer stream).
        previous = self._previous_token
        self._previous_token = token
        self._count_conditions_from_token(previous, token)
        return False

    def __call__(self, tokens, reader):
        # Custom hooks and state configurations retain the generic protocol.
        # TypeScriptReader.process_token only adds interpolation conditions
        # and always returns False, so the fast path may keep calling it.
        # Instance or subclass hooks still fall back to CodeReader.__call__.
        states = self.parallel_states
        allowed_process = (
            CodeReader.process_token, TypeScriptReader.process_token)
        if (type(states) not in (list, tuple) or len(states) != 1
                or type(states[0]) is not TypeScriptStates
                or getattr(states[0], '_binding_candidate', False) is not None
                or type(self).process_token not in allowed_process
                or getattr(self.process_token, '__func__', None)
                not in allowed_process):
            yield from CodeReader.__call__(self, tokens, reader)
            return
        self.context = reader.context
        state = states[0]
        # Binding candidates are children and retain their own wrapper.
        consume = CodeStateMachine.__call__.__get__(state)
        previous = self._previous_token
        # Bind once on this hot path: ordinary JS/TS tokens pay only the
        # after_interpolation / '${' checks (no per-token helper call).
        # TSX/JSX keep the atomic backtick scan on the rare path.
        after_interpolation = previous == '${'
        scan_atomic = type(self)._scan_atomic_templates
        for token in tokens:
            if after_interpolation:
                self._count_interpolation_conditions(token, True)
                after_interpolation = False
            elif token == '${':
                after_interpolation = True
            elif scan_atomic and token[:1] == '`' and '${' in token:
                self._count_interpolation_conditions(token, False)
            previous = token
            consume(token)
            yield token
        self._previous_token = previous
        state.statemachine_before_return()
        self.eof()

    def preprocess(self, tokens):
        """Drop spaces, and offer each line's first code token to the
        innermost open expression arrow (or optimistic signature) before
        any counter charges it.  Offer the code token after a signature to
        its state in the same way.  Mark a keyword after '.' or '?.' as the
        name of a member, and a keyword that the next token shows to be one.

        A '\n' is consumed by line_counter, and comments by comment_counter,
        before the state machine sees them, so expression_arrows cannot
        change between a newline and the next code token."""
        arrows = self.context.expression_arrows = []
        signatures = self.context.ended_signatures = []
        line_start = False
        state = self.parallel_states[0]
        marked_punctuation = _MARKED_PUNCTUATION
        member_access_tokens = _MEMBER_ACCESS
        structure_keywords = (_STRUCTURE_KEYWORDS | self.control_flow_keywords
                              | self.case_keywords)
        # `{ function: f }` also needs the mark: the state reads `function`.
        name_keywords = structure_keywords | _NAMED_KEYWORDS
        member_access = False
        before_dot = ''
        keyword = None      # waits for the next token: `{ if: a }`
        tokens = iter(self.code_tokens(tokens))
        while True:
            for token in tokens:
                if not token.isspace():
                    if keyword is not None:
                        # `{ if: a }`, `catch = f`, `catch(e) {}` in a class
                        if token in _NAME_FOLLOWERS or (
                                token[0] in _PARAMETER_STARTS
                                and state.member_name_context()):
                            keyword = _MemberName(keyword)
                        yield keyword
                        keyword = None
                    if line_start and not token.startswith(_COMMENT_STARTS):
                        line_start = False
                        end_before_line_token(state, arrows, token)
                    if signatures and not token.startswith(('//', '/*')):
                        signatures.pop().end_signature_before(token)
                    if member_access and token in structure_keywords:
                        token = _MemberName(token)
                        member_access = False
                    elif token in marked_punctuation:
                        # `1..catch`: the number and each dot are tokens.
                        member_access = token in member_access_tokens
                        if token == '?':

                            if (self.context.class_field_annotations
                                    and state.optional_member_context()):
                                token = _OPTIONAL_MEMBER_QUESTION
                        elif token == ':' and state.last_token == ')':
                            annotation = state.return_annotation_context()
                            if annotation is not None:
                                depth, unambiguous_signature = annotation
                                if unambiguous_signature:
                                    token = ReturnTypeColon(token)
                                else:
                                    tokens, lookahead = tee(tokens)
                                    if return_type_colon(lookahead, depth):
                                        token = ReturnTypeColon(token)
                                    del lookahead
                                    yield token
                                    # Resume with the buffered iterator.
                                    break
                        elif member_access:
                            before_dot = token == '.' and state.last_token
                    else:
                        if member_access and not token.startswith(
                                _COMMENT_STARTS):   # `p./* note */catch`
                            member_access = False
                        if token in name_keywords:
                            keyword = token
                            continue
                    yield token
                elif token == '\n':
                    if keyword is not None:
                        yield keyword
                        keyword = None
                    if member_access and _ends_a_number(before_dot):
                        # `1.` before a line break ends a number. On one
                        # line, `1 .if` and `1.5.if` name a member.
                        member_access = False
                    if arrows or awaiting_function_body(state):
                        line_start = True
                    yield token
            else:
                if keyword is not None:
                    yield keyword
                return

    @staticmethod
    def code_tokens(tokens):
        """The tokens that are code; embedding readers (Vue) narrow them."""
        return tokens

    @staticmethod
    @js_style_regex_expression
    def generate_tokens(source_code, addition='', token_class=None):
        # Line comment, private method (#), optional name (?), dollar
        # identifiers, nullish (??).
        addition = (addition + "|" + LINE_COMMENT + r"|(?:#\w+)"
                    + OPTIONAL_NAME + JS_IDENTIFIER + NULLISH_COALESCING
                    + OPTIONAL_ACCESS)
        yield from template_tokens(source_code, addition, token_class)
