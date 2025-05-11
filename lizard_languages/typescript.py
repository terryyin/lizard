'''
Language parser for JavaScript
'''

import re
from .code_reader import CodeReader, CodeStateMachine
from .clike import CCppCommentsMixin
from .js_style_language_states import JavaScriptStyleLanguageStates
from .js_style_regex_expression import js_style_regex_expression


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
    _conditions = set(['if', 'elseif', 'for', 'while', '&&', '||', '?',
                       'catch', 'case'])

    def __init__(self, context):
        super().__init__(context)
        self.parallel_states = [TypeScriptStates(context)]

    @staticmethod
    @js_style_regex_expression
    def generate_tokens(source_code, addition='', token_class=None):
        # Restore original addition pattern for template literals
        addition = addition + r"|(?:\$\w+)" + r"|(?:\w+\?)" + r"|`.*?`"
        for token in CodeReader.generate_tokens(source_code, addition, token_class):
            # Targeted post-processing for template literals with a single ${...}
            if (
                isinstance(token, str)
                and token.startswith('`')
                and '${' in token
                and token.endswith('`')
            ):
                # Try to split only if exactly one ${...} and one } before the ending `
                start = token.find('${')
                end = token.rfind('}')
                if start != -1 and end != -1 and start < end:
                    before = token[:start+2]  # include ${
                    inside = token[start+2:end]
                    after = token[end:]
                    yield before
                    yield inside
                    yield after
                    continue
            yield token


class TypeScriptStates(JavaScriptStyleLanguageStates):
    def __init__(self, context):
        super().__init__(context)

    def _state_global(self, token):
        if not self.as_object:
            if token == ':':
                self._consume_type_annotation()
                return
        super()._state_global(token)

    def _expecting_func_opening_bracket(self, token):
        if token == ':':
            self._consume_type_annotation()
        else:
            super()._expecting_func_opening_bracket(token)

    def _consume_type_annotation(self):
        typeStates = TypeScriptTypeAnnotationStates(self.context)

        def callback():
            if typeStates.saved_token:
                self(typeStates.saved_token)
        self.sub_state(typeStates, callback)


class TypeScriptTypeAnnotationStates(CodeStateMachine):
    def __init__(self, context):
        super().__init__(context)
        self.saved_token = None

    def _state_global(self, token):
        if token == '{':
            self.next(self._inline_type_annotation, token)
        else:
            self.next(self._state_simple_type, token)

    def _state_simple_type(self, token):
        if token == '<':
            self.next(self._state_generic_type, token)
        elif token in '{=;':
            self.saved_token = token
            self.statemachine_return()
        elif token == '(':
            self.next(self._function_type_annotation, token)

    @CodeStateMachine.read_inside_brackets_then("{}")
    def _inline_type_annotation(self, _):
        self.statemachine_return()

    @CodeStateMachine.read_inside_brackets_then("<>")
    def _state_generic_type(self, token):
        self.statemachine_return()
        
    @CodeStateMachine.read_inside_brackets_then("()")
    def _function_type_annotation(self, _):
        self.statemachine_return()
