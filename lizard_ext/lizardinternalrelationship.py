'''
This is an extension of lizard, that counts the
internal relationship of a class.
'''
from lizard_ext.keywords import IGNORED_WORDS


def delayed_tokens(tokens):
    def extend_(tokens, extendee):
        for token in tokens:
            yield token
        yield extendee

    previous = None
    for token in extend_(tokens, None):
        if previous:
            yield previous, token
        previous = token


def preprocess(tokens):
    saved_token = ""
    for token, next_token in delayed_tokens(tokens):
        if "::" in (token, next_token):
            saved_token += token
        else:
            yield saved_token + token
            saved_token = ""


class FunctionDependenciesCounter(object):
    # pylint: disable=R0903

    def __init__(self, function_builder):
        self.function_builder = function_builder
        self.potential_type = None
        self._state = self._global_state
        self._local_variables = set()
        self.function_processor = None

    def __call__(self, tokens):
        for token in preprocess(tokens):
            if not hasattr(self.function_builder, "dependencies"):
                self.function_builder.dependencies = set()
            self._state(token)
            yield token

    def _global_state(self, token):
        if token[0].isalpha() and token not in IGNORED_WORDS\
             and token not in self._local_variables\
             and token not in self.function_builder.parameters:
            self.potential_type = token
            self._state = self._potential_local_variable_state

    def _potential_local_variable_state(self, token):
        if not token[0].isalpha():
            self.function_builder.dependencies.add(
                self.potential_type)
        else:
            self._local_variables.add(token)
        self._state = self._global_state


class LizardExtension(object):  # pylint: disable=R0903
    # pylint: disable=R0903

    class StopSplitting(BaseException):
        pass

    def __call__(self, tokens, reader):
        try:
            while True:
                if reader.is_in_function():
                    for token in subcall(
                            self.wild(tokens, reader), reader):
                        yield token
                yield next(tokens)
        except self.StopSplitting:
            pass

    def wild(self, tokens, reader):
        try:
            while reader.is_in_function():
                yield next(tokens)
        except StopIteration:
            raise self.StopSplitting


def subcall(tokens, reader):
    fdc = FunctionDependenciesCounter(reader.context.current_function)
    return fdc(tokens)
