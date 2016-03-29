'''
Language parser for C Sharp
'''

from .clike import CLikeReader


class CSharpReader(CLikeReader):
    # pylint: disable=R0903

    ext = ['cs']
    language_names = ['csharp']

    conditions = set(['if', 'for', 'while', '&&', '||', '?', 'catch',
                      'case', '??'])

    def __init__(self, context):
        super(CSharpReader, self).__init__(context)

    @staticmethod
    def generate_tokens(source_code, _=''):
        return CLikeReader.generate_tokens(source_code, r"|(?:\?\?)")
