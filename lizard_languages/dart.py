'''
Language parser for Dart
'''

from .clike import CLikeReader, CLikeStates, CLikeNestingStackStates


class DartReader(CLikeReader):
    # pylint: disable=R0903

    ext = ['dart']
    language_names = ['dart']

    _logical_operators = {'&&', '||', '??', '??='}

    def __init__(self, context):
        super(DartReader, self).__init__(context)
        self.parallel_states = [
            DartStates(context),
            DartNestingStackStates(context)
        ]

    @staticmethod
    def generate_tokens(source_code, addition='', token_class=None):
        # Longest first, subgroup-free: triple/raw strings ahead of base
        # quotes; then nullable names, ??=, ??, ?.., ?.
        # `(?!\.)` keeps int?/T? from swallowing the ? of ?. and ?..
        # Closing ?> generics stay the existing <…?> token.
        return CLikeReader.generate_tokens(
            source_code,
            r"|(?:r)?'''[\s\S]*?'''" +
            r'|(?:r)?"""[\s\S]*?"""' +
            r"|r'(?:\\.|[^'\\])*?'" +
            r'|r"(?:\\.|[^"\\])*"' +
            r"|\w+\?(?!\.)" +
            r"|\?\?=" +
            r"|\?\?" +
            r"|\?\.\." +
            r"|\?\." +
            addition,
            token_class
        )


class DartStates(CLikeStates):  # pylint: disable=R0903
    pass


class DartNestingStackStates(CLikeNestingStackStates):  # pylint: disable=R0903
    """Nesting for Dart types, including mixin and enum (not extension)."""

    _TYPE_KEYWORDS = CLikeNestingStackStates._TYPE_KEYWORDS + ("mixin", "enum")
