"""
This extension dumps all the comments in one file
"""


DEFAULT_NS_THRESHOLD = 3


class LizardExtension(object):  # pylint: disable=R0903

    silent_all_others = True
    ordering_index = 1

    def __call__(self, tokens, reader):
        print("=" * 20)
        print(reader.context.fileinfo.filename)
        print("-" * 20)
        for token in tokens:
            comment = reader.get_comment_from_token(token)
            if comment is not None:
                print(comment)
            yield token
