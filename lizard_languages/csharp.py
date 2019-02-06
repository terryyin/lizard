'''
Language parser for C Sharp
'''

from .clike import CLikeReader


class CSharpReader(CLikeReader):
    # pylint: disable=R0903

    ext = ['cs']
    language_names = ['csharp']

    _conditions = set(['if', 'for', 'while', '&&', '||', '?', 'catch',
                      'case', '??'])

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        return CLikeReader.generate_tokens(
                source_code, r"|(?:\?\?)", token_class)
