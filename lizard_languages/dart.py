'''
Language parser for Dart
'''

from .clike import CLikeReader, CLikeStates, CLikeNestingStackStates


class DartReader(CLikeReader):
    # pylint: disable=R0903

    ext = ['dart']
    language_names = ['dart']

    def __init__(self, context):
        super(DartReader, self).__init__(context)
        self.parallel_states = [
            DartStates(context),
            DartNestingStackStates(context)
        ]


class DartStates(CLikeStates):  # pylint: disable=R0903
    pass


class DartNestingStackStates(CLikeNestingStackStates):  # pylint: disable=R0903
    """Nesting for Dart types, including mixin and enum (not extension)."""

    _TYPE_KEYWORDS = CLikeNestingStackStates._TYPE_KEYWORDS + ("mixin", "enum")
