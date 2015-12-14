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
