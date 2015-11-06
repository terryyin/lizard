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


class LizardExtension(object):  # pylint: disable=R0903

    def __init__(self):
        self.potential_type = None
        self._state = self._global_state
        self._local_variables = set()

    def preprocess(self, tokens):
        saved_token = ""
        for token, next_token in delayed_tokens(tokens):
            if "::" in (token, next_token):
                saved_token += token
            else:
                yield saved_token + token
                saved_token = ""

    def __call__(self, tokens, reader):
        for token in self.preprocess(tokens):
            if not hasattr(reader.context.current_function, "dependencies"):
                reader.context.current_function.dependencies = set()
            if reader.is_in_function():
                self._state(token, reader)
            yield token

    def _global_state(self, token, reader):
        if token[0].isalpha() and token not in IGNORED_WORDS\
             and token not in self._local_variables:
            self.potential_type = token
            self._state = self._potential_local_variable_state

    def _potential_local_variable_state(self, token, reader):
        if not token[0].isalpha():
            reader.context.current_function.dependencies.add(
                self.potential_type)
        else:
            self._local_variables.add(token)
        self._state = self._global_state
