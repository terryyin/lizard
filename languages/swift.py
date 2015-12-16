'''
Language parser for Apple Swift
'''

from lizard import CodeReader, CCppCommentsMixin


class SwiftReader(CodeReader, CCppCommentsMixin):
    # pylint: disable=R0903

    ext = ['swift']
    language_names = ['swift']

    def __init__(self, context):
        super(SwiftReader, self).__init__(context)
        self._state = self._global
        self.br_count = 0

    def _global(self, token):
        if token == 'func':
            self._state = self._function_name

    def _function_name(self, token):
        self.context.start_new_function(token)
        self._state = self._expect_function_dec

    def _expect_function_dec(self, token):
        if token == '(':
            self._state = self._function_dec

    def _function_dec(self, token):
        if token == ')':
            self._state = self._expect_function_impl
        else:
            self.context.parameter(token)

    def _expect_function_impl(self, token):
        if token == 'func':
            self._state = self._function_name
        if token == '{':
            self.br_count += 1
            self._state = self._function_impl

    @CodeReader.read_brackets
    def _function_impl(self, _):
        self._state = self._global
        self.context.end_of_function()
