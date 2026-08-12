"""Operator/operand classification for Halstead metrics."""

import keyword


class HalsteadClassifier(object):
    """Classify tokens into Halstead operators and operands.

    This is the per-language extension point.  The base class implements the
    generic rules that work for the operators and punctuation shared by
    lizard's common tokenizer, plus a broad set of C-family keywords.  Subclass
    it and override :attr:`keyword_operators` / :attr:`literal_keywords` (or
    :meth:`classify` itself) for language-specific behaviour.
    """

    OPERATOR = "operator"
    OPERAND = "operand"
    SKIP = None

    #: Keywords that behave as operators (control flow, declarations, ...).
    keyword_operators = frozenset({
        "if", "else", "for", "while", "do", "switch", "case", "default",
        "break", "continue", "return", "goto", "try", "catch", "finally",
        "throw", "throws", "int", "long", "short", "char", "float", "double",
        "void", "bool", "boolean", "unsigned", "signed", "const", "static",
        "extern", "register", "volatile", "auto", "struct", "union", "enum",
        "class", "typedef", "public", "private", "protected", "virtual",
        "inline", "template", "typename", "namespace", "using", "new",
        "delete", "sizeof", "operator", "import", "package", "extends",
        "implements", "interface", "synchronized", "function", "var", "let",
        "typeof", "instanceof", "in", "of", "await", "async", "yield", "and",
        "or", "not", "xor",
    })

    #: Keywords that denote literal values (counted as operands).
    literal_keywords = frozenset({
        "true", "false", "null", "nil", "none", "nullptr", "undefined",
    })

    def classify(self, token):
        """Return ``OPERATOR``, ``OPERAND`` or ``SKIP`` for ``token``."""
        if not token or token.isspace():
            return self.SKIP
        first = token[0]
        if first in "\"'" or first.isdigit() or \
            (first == "." and token[1:2].isdigit()):
            # A string or numeric literal: lizard emits each quoted run or
            # number (or number fragment) as a single token.
            return self.OPERAND
        if first.isalpha() or first == "_":
            # A word: keyword or identifier.
            if token in self.literal_keywords:
                return self.OPERAND
            if token in self.keyword_operators:
                return self.OPERATOR
            return self.OPERAND
        # Anything else is operator/punctuation.
        return self.OPERATOR

    def classify_with_next(self, token, next_token):
        """Classify ``token``, optionally using the following token.

        The base implementation ignores ``next_token``.  Language-specific
        classifiers may override this when a one-token look-ahead is required
        (for example Python string prefixes).
        """
        return self.classify(token)

    def needs_next_token(self, token):
        """Return True if classifying ``token`` should wait for the next token."""
        return False


class PythonHalsteadClassifier(HalsteadClassifier):
    """Precise operator/operand classification for Python.

    Every hard keyword is an operator except the value literals ``True``,
    ``False`` and ``None``.  Soft keywords (``match``, ``case``, ``type``,
    ``_``) are context dependent and are treated as ordinary identifiers, i.e.
    operands.  Ellipsis (``...``) is a value literal and counted as an
    operand.  String prefixes (``f``, ``b``, ``r``, ...) that lizard emits as
    a separate token immediately before a quoted string are skipped so they
    do not inflate the operand vocabulary; the same letters used as ordinary
    identifiers still count as operands.
    """

    literal_keywords = frozenset({"True", "False", "None"})
    keyword_operators = frozenset(keyword.kwlist) - literal_keywords

    #: Prefixes lizard may emit as their own token before a string literal.
    STRING_PREFIXES = frozenset({
        'r', 'u', 'f', 'b',
        'fr', 'rf', 'br', 'rb', 'bf', 'fb',
    })

    def classify(self, token):
        if token == '...':
            return self.OPERAND
        return super().classify(token)

    def needs_next_token(self, token):
        return self._is_string_prefix(token)

    def classify_with_next(self, token, next_token):
        if next_token and next_token[:1] in "\"'" and self._is_string_prefix(token):
            return self.SKIP
        return self.classify(token)

    def _is_string_prefix(self, token):
        return token.lower() in self.STRING_PREFIXES


#: Registry of language-specific classifiers, keyed by lower-case language name.
_CLASSIFIERS = {
    "python": PythonHalsteadClassifier,
}


def get_classifier(reader):
    """Pick the classifier for ``reader``.

    Resolution order:

    1. a ``halstead_classifier`` provided by the language reader itself,
    2. a classifier registered for one of the reader's ``language_names``,
    3. the generic :class:`HalsteadClassifier`.
    """
    classifier = getattr(reader, "halstead_classifier", None)
    if classifier is not None:
        return classifier
    for name in getattr(reader, "language_names", None) or []:
        registered = _CLASSIFIERS.get(name.lower())
        if registered is not None:
            return registered()
    return HalsteadClassifier()
