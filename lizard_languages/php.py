'''
Language parser for PHP
'''

import re
from .code_reader import CodeReader
from .clike import CCppCommentsMixin
from .php_states import PHPLanguageStates
from .token_pattern import compiled_token_pattern


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
        # PHP comparisons must not use the shared generic angle-bracket rule.
        addition += r"|<<=|<=|<"
        # A closing tag ends a line comment, but not a string or block comment.
        addition += r"|(?://|\#)[^\n]*?(?=\?>|\n|\Z)|\?>"
        pattern = compiled_token_pattern(addition)
        current_pos = 0
        opening_tag = re.compile(r"\<\?(?:php)?")
        while match := opening_tag.search(source_code, current_pos):
            if source_code[current_pos:match.start()]:
                yield '"' + source_code[current_pos:match.start()] + '"'
            for token_match in pattern.finditer(source_code, match.end()):
                if token_match.group(0) == '?>':
                    current_pos = token_match.end()
                    break
                yield token_class(token_match) if token_class else token_match.group(0)
            else:
                return
        if source_code[current_pos:]:
            yield '"' + source_code[current_pos:] + '"'

    def __init__(self, context):
        super(PHPReader, self).__init__(context)
        self.parallel_states = [PHPLanguageStates(context)]
