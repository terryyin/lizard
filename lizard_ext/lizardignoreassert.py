'''
This is an extension of lizard, that ignores the CCN within
the assertion.
'''


class _IgnoredAnd(str):
    is_ignored_condition = True


_IGNORED_AND = _IgnoredAnd("&&")


class LizardExtension:  # pylint: disable=R0903
    _PARENTHESES = {"(": 1, ")": -1}
    _ASSERTION_CONDITIONS = {"&&", "||", "?", "??"}

    def __call__(self, tokens, reader=None):
        tokens = iter(tokens)
        for token in tokens:
            yield token
            if token in ("assert", "static_assert"):
                yield from self._assertion(tokens, reader)

    def _assertion(self, tokens, reader):
        depth = 0
        conditions = reader.conditions
        context = reader.context
        for token in tokens:
            depth += self._PARENTHESES.get(token, 0)
            if token in self._ASSERTION_CONDITIONS and token in conditions and not getattr(token, 'is_optional', False):
                context.add_condition(-1)
                if token == "&&":
                    token = _IGNORED_AND
            yield token
            if depth == 0:
                return
