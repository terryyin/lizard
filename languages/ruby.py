'''
Language parser for JavaScript
'''

from lizard import CodeReader
from lizard import CLikeReader


class RubyReader(CLikeReader, CodeReader):
    # pylint: disable=R0903

    ext = ['rb']
    language_names = ['ruby']

    def __init__(self, context):
        super(RubyReader, self).__init__(context)
