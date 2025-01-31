'''
Language parser for Vue.js
'''

from .typescript import TypeScriptReader, TypeScriptStates
from .javascript import JavaScriptReader, JSTokenizer, isidentifier
from .code_reader import CodeReader
from .clike import CCppCommentsMixin
from .js_style_regex_expression import js_style_regex_expression
from .js_style_language_states import JavaScriptStyleLanguageStates, ES6ObjectStates


class VueReader(CodeReader, CCppCommentsMixin):
    # pylint: disable=R0903

    ext = ['vue']
    language_names = ['vue']

    @staticmethod
    @js_style_regex_expression
    def generate_tokens(source_code, addition='', token_class=None):
        # Extract script content between <script> tags
        script_content = ''
        script_tag = ''
        in_script = False
        for line in source_code.split('\n'):
            if '<script' in line:
                in_script = True
                script_tag = line
                continue
            if '</script>' in line:
                break
            if in_script:
                script_content += line + '\n'

        # If no script content found, return empty generator
        if not script_content:
            return iter(())

        # Add common JavaScript/TypeScript patterns
        addition = addition + r"|(?:\$\w+)|`.*?`"

        # For TypeScript, add type-specific patterns
        if 'lang="ts"' in script_tag or 'lang="typescript"' in script_tag:
            addition = addition + r"|(?:\w+\?)|(?::\s*\w+)"

        # Use JavaScript tokenizer for proper token handling
        js_tokenizer = JSTokenizer()
        for token in CodeReader.generate_tokens(script_content, addition, token_class):
            for tok in js_tokenizer(token):
                yield tok

    def __init__(self, context):
        super(VueReader, self).__init__(context)
        self.context = context
        self.context.script_tag = None
        self.parallel_states = [VueStates(context)]

    def __call__(self, tokens, reader):
        # Convert tokens to list to avoid exhausting the iterator
        token_list = list(tokens)

        # Extract script tag from the tokens and store in context
        for token in token_list:
            if token.startswith('<script'):
                self.context.script_tag = token
                break
            if token == '</script>':
                break

        # Set up the appropriate state machine based on the script tag
        if self.context.script_tag and ('lang="ts"' in self.context.script_tag or 'lang="typescript"' in self.context.script_tag):
            self.parallel_states = [TypeScriptStates(self.context)]
        else:
            self.parallel_states = [VueStates(self.context)]

        # Use the base class's __call__ method to process tokens
        return super(VueReader, self).__call__(iter(token_list), reader)


class VueStates(TypeScriptStates):
    def _state_global(self, token):
        if token.startswith('<script'):
            self.next(self._process_script_content)
            return
        if token.startswith('<template'):
            self.next(self._process_template_content)
            return
        if token == '{':
            if self.started_function:
                self.sub_state(
                    TypeScriptStates(self.context),
                    self._pop_function_from_stack)
            else:
                self.read_object()
            return
        super(VueStates, self)._state_global(token)

    def _process_script_content(self, token):
        if token == '</script>':
            self.next(self._state_global)
        else:
            # Delegate to TypeScript/JavaScript states
            super(VueStates, self)._state_global(token)

    def _process_template_content(self, token):
        if token == '</template>':
            self.next(self._state_global)

    def read_object(self):
        # Check the stored script tag instead of context.current_file
        if self.context.script_tag and ('lang="ts"' in self.context.script_tag or 'lang="typescript"' in self.context.script_tag):
            self.sub_state(TypeScriptObjectStates(self.context))
        else:
            self.sub_state(ES6ObjectStates(self.context))


class TypeScriptObjectStates(ES6ObjectStates):
    def _state_global(self, token):
        if token == ':':
            self.function_name = self.last_tokens
        elif token == '(':
            self._function(self.last_tokens)
            self.next(self._function, token)
        elif token == ')':
            self.next(self._function_return_type)
        else:
            super(TypeScriptObjectStates, self)._state_global(token)

    def _function_return_type(self, token):
        if token == '{':
            self.next(self._state_global, token)
        elif token == ';':
            self.next(self._state_global)
        else:
            # Skip over the return type and continue
            self.next(self._function_return_type) 