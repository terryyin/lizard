'''
Language parser for JavaScript
'''

import re
from .code_reader import CodeReader
from .clike import CCppCommentsMixin
from .js_style_language_states import JavaScriptStyleLanguageStates, ES6ObjectStates
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
        super(JSTokenizer, self).__init__()
        self.depth = 1

    def process_token(self, token):
        if token == "<":
            from .jsx import XMLTagWithAttrTokenizer  # Import only when needed
            self.sub_tokenizer = XMLTagWithAttrTokenizer()
            return
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
        super(TypeScriptReader, self).__init__(context)
        self.parallel_states = [TypeScriptStates(context)]

    @staticmethod
    @js_style_regex_expression
    def generate_tokens(source_code, addition='', token_class=None):
        addition = addition +\
            r"|(?:\$\w+)" + \
            r"|(?:\w+\?)" + \
            r"|`.*?`"
        js_tokenizer = JSTokenizer()
        for token in CodeReader.generate_tokens(
                source_code, addition, token_class):
            for tok in js_tokenizer(token):
                yield tok


class TypeScriptStates(JavaScriptStyleLanguageStates):
    def __init__(self, context):
        super(TypeScriptStates, self).__init__(context)
        self._in_type_declaration = False

    def _state_global(self, token):
        if token == ':':
            # When we see a type annotation in global state, store the last tokens
            # but don't treat it as a function name yet
            self._potential_name = self.last_tokens
            self._in_type_declaration = True
            self.next(self._type_annotation)
            return
        if token == '=>':
            # For arrow functions, preserve the name if it was assigned
            if hasattr(self, 'function_name') and self.function_name:
                pass  # Keep the existing function name from assignment
            else:
                self.function_name = ''
            self._state = self._arrow_function
            return
        super(TypeScriptStates, self)._state_global(token)

    def _type_annotation(self, token):
        if token == '=':
            # We're back to an assignment, restore the potential name
            if hasattr(self, '_potential_name'):
                self.last_tokens = self._potential_name
                delattr(self, '_potential_name')
            self._in_type_declaration = False
            self.next(self._state_global, token)
        else:
            self.next(self._type_annotation)

    def _expecting_func_opening_bracket(self, token):
        if token == ':':
            self._in_type_declaration = True
            self.next(self._expecting_default)
            return
        super(TypeScriptStates, self)._expecting_func_opening_bracket(token)

    def _expecting_default(self, token):
        self.next(self._function_return_type)
        if token == '{' and not self._in_type_declaration:
            self.read_object()

    def _function_return_type(self, token):
        if token == ';':
            self._in_type_declaration = False
            self.next(self._state_global)
        elif token == '{':
            self._in_type_declaration = False
            self.next(self._expecting_func_opening_bracket, token)
        elif token == '=':
            self._in_type_declaration = False
            self.next(self._state_global, token)

    def read_object(self):
        if not self._in_type_declaration:
            self.sub_state(TypeScriptObjectStates(self.context))
        else:
            # Skip object type definitions by treating them as part of type annotation
            self.next(self._type_annotation)


class TypeScriptObjectStates(ES6ObjectStates):
    def _state_global(self, token):
        if token == ':' and self.last_tokens:
            # Only treat it as a function if it's followed by a type annotation
            # Don't treat simple object properties as functions
            self.next(self._property_or_method)
            return
        elif token == '=>' and hasattr(self, 'function_name') and self.function_name:
            self._push_function_to_stack()
            self.next(self._state_global)
            return
        elif token == '{':
            # Handle nested objects
            self.read_object()
            return
        elif token == '(' and self.last_tokens:
            # This is a method declaration
            self.function_name = self.last_tokens
            self._push_function_to_stack()
            self.next(self._state_global)
            return
        super(TypeScriptObjectStates, self)._state_global(token)

    def _property_or_method(self, token):
        if token == '{':
            # If we see an opening brace after a colon, it's an object
            self.read_object()
            self.next(self._state_global)
        else:
            # Skip the type annotation or value
            self.next(self._state_global)

    def _method_return_type(self, token):
        if token == '{':
            self._push_function_to_stack()
            self.next(self._state_global, token)
        elif token == '=>':
            self._push_function_to_stack()
            self.next(self._state_global)
        else:
            self.next(self._method_return_type)
