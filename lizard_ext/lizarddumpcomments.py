"""
This extension dumps all the comments in one file
"""
from __future__ import print_function, division


DEFAULT_NS_THRESHOLD = 3


class LizardExtension(object):  # pylint: disable=R0903

    silent_all_others = True
    ordering_index = 1

    def __call__(self, tokens, reader):
        first = True
        print("=" * 20)
        print(reader.context.fileinfo.filename)
        print("-" * 20)
        for token in tokens:
            comment = reader.get_comment_from_token(token)
            if comment is not None:
                if not first or "copyright" not in comment.lower():
                    print(comment)
                first = False
            yield token
