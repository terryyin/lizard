'''
Language parser for C Sharp
'''

from .clike import CLikeReader


class CSharpReader(CLikeReader):
    # pylint: disable=R0903

    ext = ['cs']
    language_names = ['csharp']

    def __init__(self, context):
        super(CSharpReader, self).__init__(context)
