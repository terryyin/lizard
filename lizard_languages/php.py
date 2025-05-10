'''
Language parser for JavaScript
'''

import re
from .code_reader import CodeReader, CodeStateMachine
from .clike import CCppCommentsMixin
from .js_style_language_states import JavaScriptStyleLanguageStates


class PHPLanguageStates(JavaScriptStyleLanguageStates):
    """
    PHP-specific language state machine that handles modern PHP syntax
    including visibility modifiers and return types.
    """
    
    def __init__(self, context):
        super(PHPLanguageStates, self).__init__(context)
        self.current_visibility = None
        self.is_static = False
        self.reading_function = False
        
    def _state_global(self, token):
        if token in ('public', 'private', 'protected'):
            self.current_visibility = token
            return
        elif token == 'static':
            self.is_static = True
            return
        elif token == 'function':
            self.reading_function = True
            self._state = self._function
            return
        elif token in ('foreach',):
            # Handle PHP foreach as a control structure, not a function
            self.next(self._expecting_condition_and_statement_block)
            return
            
        # Reset these flags when we encounter other tokens
        if token not in ('=>', '=', ';', '\n'):
            self.current_visibility = None
            self.is_static = False
        
        # Now delegate to the parent class
        super(PHPLanguageStates, self)._state_global(token)
        
    def _function(self, token):
        if token in ('*', ':'):
            # Skip * (for generators) and : (for return type)
            return
        if token != '(':
            self.function_name = token
            self.reading_function = False
        else:
            self._push_function_to_stack()
            self._state = self._dec
            if token == '(':
                self._dec(token)


class PHPReader(CodeReader, CCppCommentsMixin):
    # pylint: disable=R0903

    ext = ['php']
    language_names = ['php']
    _conditions = set(['if', 'elseif', 'for', 'foreach', 'while', '&&', '||', '?',
                       'catch', 'case'])

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        addition += r"|(?:\$\w+)"
        addition += r"|(?:\<{3}(?P<quote>\w+).*?(?P=quote))"
        current_pos = 0
        code_block_pattern = re.compile(
                r"\<\?(?:php)?(.*?)(?:(\?\>)|\Z)",
                re.M | re.S)
        for match in code_block_pattern.finditer(source_code):
            if source_code[current_pos:match.start()]:
                yield '"' + source_code[current_pos:match.start()] + '"'
            for token in CodeReader.generate_tokens(
                    match.group(1), addition, token_class):
                yield token
            current_pos = match.end()
        if source_code[current_pos:]:
            yield '"' + source_code[current_pos:] + '"'

    def __init__(self, context):
        super(PHPReader, self).__init__(context)
        self.parallel_states = [PHPLanguageStates(context)]
