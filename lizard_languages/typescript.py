'''
Language parser for TypeScript
'''

from itertools import tee

from .code_reader import CodeReader, CodeStateMachine
from .clike import CCppCommentsMixin
from .js_style_regex_expression import js_style_regex_expression
from .javascript_bindings import JavaScriptBindings
from .typescript_declarations import TypeScriptTypeAnnotationStates
from .typescript_states import TypeScriptStates
from .typescript_signatures import ReturnTypeColon, return_type_colon
from .javascript_templates import LINE_COMMENT, interpolation_code, template_tokens

# One token, so each ? in ?? is not its own ternary decision.
NULLISH_COALESCING = r"|(?:\?\?)"
OPTIONAL_ACCESS = r"|(?:\?\.(?!\d))"
# An optional member or parameter, "name?: T" or "f(a?)"; in "x?y:0" the
# "?" opens a ternary.
OPTIONAL_NAME = r"|(?:\w+\?(?=\s*(?:[:,)]|\Z)))"
# JavaScript identifiers can contain dollar signs as well as word characters.
JS_IDENTIFIER = r"|(?:[\w$]+)"


class Tokenizer(object):
    def __init__(self):
        self.sub_tokenizer = None
        self._ended = False

    def __call__(self, token):
        if self.sub_tokenizer:
            for tok in self.sub_tokenizer(token):
                yield tok
            if self.sub_tokenizer._ended:
                self.sub_tokenizer = None
            return
        for tok in self.process_token(token):
            yield tok

    def stop(self):
        self._ended = True

    def process_token(self, token):
        pass


class JSTokenizer(Tokenizer):
    def __init__(self):
        super().__init__()
        self.depth = 1

    def process_token(self, token):
        if token == "{":
            self.depth += 1
        elif token == "}":
            self.depth -= 1
            if self.depth == 0:
                self.stop()
                return
        yield token


class TypeScriptReader(CodeReader, CCppCommentsMixin):
    # pylint: disable=R0903

    ext = ['ts']
    language_names = ['typescript', 'ts']
    # JS/TS split templates into '${' + expression tokens; only TSX/JSX keep
    # atomic templates that need a backtick scan on the fast path.
    _scan_atomic_templates = False

    # Separated condition categories
    _control_flow_keywords = {'if', 'elseif', 'for', 'while', 'catch'}
    _logical_operators = {'&&', '||', '??'}
    _case_keywords = {'case'}
    _ternary_operators = {'?'}

    def __init__(self, context):
        super().__init__(context)
        self.parallel_states = [TypeScriptStates(context)]
        self._previous_token = None

    def _count_interpolation_conditions(self, token, in_expression):
        # Re-tokenize opaque interpolation spans; keyword members stay non-
        # conditions the same way as on the outer token stream (#546).
        conditions = self.conditions
        previous = None
        for start, end in interpolation_code(token, in_expression):
            for inner in self.generate_tokens(token[start:end]):
                if (inner in conditions
                        and previous not in ('.', '?.')):
                    self.context.add_condition()
                previous = inner

    def _count_conditions_from_token(self, previous, token):
        # Split JS/TS exposes '${' then the expression; atomic TSX/JSX
        # templates keep the whole backtick token.
        if previous == '${':
            self._count_interpolation_conditions(token, True)
        elif (type(self)._scan_atomic_templates
                and token[:1] == '`' and '${' in token):
            self._count_interpolation_conditions(token, False)

    def process_token(self, token):
        # Undo keyword member names (#546) and count conditions written
        # inside template interpolations (opaque to the outer stream).
        previous = self._previous_token
        self._previous_token = token
        if previous in ('.', '?.') and token in self.conditions:
            self.context.add_condition(-1)
        self._count_conditions_from_token(previous, token)
        return False

    def __call__(self, tokens, reader):
        # Custom hooks and state configurations retain the generic protocol.
        # TypeScriptReader.process_token only undoes keyword members / adds
        # interpolation conditions and always returns False, so the fast path
        # may keep calling it. Instance or subclass hooks still fall back to
        # CodeReader.__call__.
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
        conditions = self.conditions
        # Bind once: ordinary JS/TS tokens pay only after_interpolation / '${'
        # checks; TSX/JSX keep the atomic backtick scan on the rare path.
        after_interpolation = previous == '${'
        scan_atomic = type(self)._scan_atomic_templates
        for token in tokens:
            if previous in ('.', '?.') and token in conditions:
                self.context.add_condition(-1)
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
        innermost open expression arrow before any counter charges it.

        A '\n' is consumed by line_counter, and comments by comment_counter,
        before the state machine sees them, so expression_arrows cannot
        change between a newline and the next code token."""
        arrows = self.context.expression_arrows = []
        line_start = False
        state = self.parallel_states[0]
        tokens = iter(self.code_tokens(tokens))
        while True:
            for token in tokens:
                if not token.isspace():
                    if line_start and not token.startswith(('//', '/*')):
                        line_start = False
                        arrows[-1].end_expression_before(token)
                    if token == ':' and state.last_token == ')':
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
                                # Resume on the main iterator, including buffered tokens.
                                break
                    yield token
                elif token == '\n':
                    if arrows:
                        line_start = True
                    yield token
            else:
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
