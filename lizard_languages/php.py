'''
Language parser for PHP
'''

import re
from .code_reader import CodeReader
from .clike import CCppCommentsMixin
from .php_states import PHPLanguageStates


class PHPReader(CodeReader, CCppCommentsMixin):
    # pylint: disable=R0903

    ext = ['php']
    language_names = ['php']

    # Separated condition categories
    _control_flow_keywords = {'if', 'elseif', 'for', 'foreach', 'while', 'catch', 'match'}
    _logical_operators = {'&&', '||'}  # PHP also has 'and', 'or' with different precedence
    _case_keywords = {'case'}
    _ternary_operators = {'?'}
    # Operators that contain '?' but are not the ternary operator
    _non_ternary_question_operators = r"|(?:\?\?=)|(?:\?\?)|(?:\?->)|(?:\?:)"

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        addition += r"|(?:\$\w+)"
        addition += r"|(?:\<{3}(?P<quote>\w+).*?(?P=quote))"
        addition += PHPReader._non_ternary_question_operators
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
