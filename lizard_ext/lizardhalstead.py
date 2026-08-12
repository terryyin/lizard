"""
Halstead complexity measures for lizard (``-Ehalstead``).

Halstead metrics are derived from two basic counts taken over a unit of
code (here, every function):

* ``n1`` - the number of *distinct* operators
* ``n2`` - the number of *distinct* operands
* ``N1`` - the *total* number of operators
* ``N2`` - the *total* number of operands

From those four numbers the usual derived measures are computed: vocabulary,
length, volume, difficulty, effort, time and an estimate of delivered bugs
(see :class:`HalsteadMetrics`).

Counting convention
-------------------
Halstead numbers are notoriously sensitive to *how* you decide what counts as
an operator and what counts as an operand, so the rules used here are spelled
out explicitly to make results comparable with (or at least explainable next
to) other tools:

* Every token that lizard already emits is classified as exactly one of
  ``operator``, ``operand`` or *skipped*.  Lizard strips whitespace and
  comments before extensions see the stream, so those never participate.  This
  keeps the classification in one-to-one correspondence with the tokens that
  drive ``token_count``.
* **Operands** are identifiers (variable, function, attribute and parameter
  names), numeric literals, string literals and the literal keywords of the
  language (``True``/``False``/``None`` in Python, ``true``/``false``/``null``
  and friends elsewhere).
* **Operators** are operator and punctuation symbols (``+``, ``==``, ``.``,
  ``,``, ``:``, ``(``, ``)``, ``{``, ``}`` ...) together with the keywords that
  act as operators or control structures (``if``, ``for``, ``return``, ``def``,
  ``and`` ...).
* Each paired delimiter token is counted on its own; ``(`` and ``)`` are two
  distinct operators, each contributing one occurrence.  This follows directly
  from lizard tokenizing them separately and keeps the one-token-one-label
  property above.
* Tokens are attributed to a function exactly the way ``token_count`` is, so
  the ``def``/``class`` keyword and the function name itself belong to the
  enclosing scope rather than to the function body.

Operator/operand classification is language specific, so it lives behind a
small, explicit interface (:class:`HalsteadClassifier`).  Python ships with a
precise classifier; other languages fall back to a generic C-family classifier
that is a reasonable approximation until a language-specific one is added.  A
language reader can also provide its own by exposing a ``halstead_classifier``
attribute, which is the seam intended for folding this into the core later.
"""

import keyword
import math
from collections import Counter

from lizard import FunctionInfo


class HalsteadMetrics(object):
    """Compute the Halstead measures from operator/operand multisets.

    The object only stores the two :class:`collections.Counter` instances and
    derives everything on access, so it always reflects the final counts no
    matter when it is read.
    """

    def __init__(self, operators=None, operands=None):
        self._operators = operators if operators is not None else Counter()
        self._operands = operands if operands is not None else Counter()

    # -- the four basic counts ------------------------------------------------
    @property
    def distinct_operators(self):  # n1
        return len(self._operators)

    @property
    def distinct_operands(self):  # n2
        return len(self._operands)

    @property
    def total_operators(self):  # N1
        return sum(self._operators.values())

    @property
    def total_operands(self):  # N2
        return sum(self._operands.values())

    # -- derived measures -----------------------------------------------------
    @property
    def vocabulary(self):
        """n = n1 + n2"""
        return self.distinct_operators + self.distinct_operands

    @property
    def length(self):
        """N = N1 + N2"""
        return self.total_operators + self.total_operands

    @property
    def calculated_length(self):
        """Estimated length: n1*log2(n1) + n2*log2(n2)."""
        n_1 = self.distinct_operators
        n_2 = self.distinct_operands
        return ((n_1 * math.log2(n_1) if n_1 else 0.0) +
                (n_2 * math.log2(n_2) if n_2 else 0.0))

    @property
    def volume(self):
        """V = N * log2(n)."""
        vocabulary = self.vocabulary
        return self.length * math.log2(vocabulary) if vocabulary > 0 else 0.0

    @property
    def difficulty(self):
        """D = (n1 / 2) * (N2 / n2)."""
        n_2 = self.distinct_operands
        if n_2 == 0:
            return 0.0
        return (self.distinct_operators / 2.0) * (self.total_operands / n_2)

    @property
    def effort(self):
        """E = D * V."""
        return self.difficulty * self.volume

    @property
    def time(self):
        """Estimated programming time in seconds: E / 18."""
        return self.effort / 18.0

    @property
    def bugs(self):
        """Estimated delivered bugs: V / 3000."""
        return self.volume / 3000.0

    def as_dict(self):
        return {
            "n1": self.distinct_operators,
            "n2": self.distinct_operands,
            "N1": self.total_operators,
            "N2": self.total_operands,
            "vocabulary": self.vocabulary,
            "length": self.length,
            "calculated_length": self.calculated_length,
            "volume": self.volume,
            "difficulty": self.difficulty,
            "effort": self.effort,
            "time": self.time,
            "bugs": self.bugs,
        }


class HalsteadClassifier(object):  # pylint: disable=too-few-public-methods
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


class PythonHalsteadClassifier(HalsteadClassifier):  # pylint: disable=too-few-public-methods
    """Precise operator/operand classification for Python.

    Every hard keyword is an operator except the value literals ``True``,
    ``False`` and ``None``.  Soft keywords (``match``, ``case``, ``type``,
    ``_``) are context dependent and are treated as ordinary identifiers, i.e.
    operands.
    """

    literal_keywords = frozenset({"True", "False", "None"})
    keyword_operators = frozenset(keyword.kwlist) - literal_keywords


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


def _function_halstead(function):
    return HalsteadMetrics(
        getattr(function, "_halstead_operators", None),
        getattr(function, "_halstead_operands", None))


#: (attribute name on FunctionInfo, measure name on HalsteadMetrics).
_HALSTEAD_MEASURES = (
    ("halstead_n1", "distinct_operators"),
    ("halstead_n2", "distinct_operands"),
    ("halstead_N1", "total_operators"),
    ("halstead_N2", "total_operands"),
    ("halstead_vocabulary", "vocabulary"),
    ("halstead_length", "length"),
    ("halstead_volume", "volume"),
    ("halstead_difficulty", "difficulty"),
    ("halstead_effort", "effort"),
    ("halstead_time", "time"),
    ("halstead_bugs", "bugs"),
)


def _make_measure_property(measure_name):
    def getter(self):
        value = getattr(self.halstead, measure_name)
        return round(value, 2) if isinstance(value, float) else value
    return property(getter)


def ensure_function_info_patched(function_info_class):
    """Expose the Halstead measures as attributes on a ``FunctionInfo`` class.

    They are read-only properties derived from the per-function operator and
    operand multisets, so they default to zero for any function that never went
    through the extension.  Being real attributes means they also work with
    ``--sort``/``--Threshold`` and the CSV/XML writers.

    The class is resolved from the instances that flow through the extension
    (``type(function)``) rather than from a single imported reference, so this
    keeps working when lizard is run as ``python -m lizard`` (where the module
    also exists as ``__main__``) or across multiprocessing workers.
    """
    if function_info_class.__dict__.get("_halstead_patched"):
        return
    function_info_class.halstead = property(_function_halstead)
    for attribute_name, measure_name in _HALSTEAD_MEASURES:
        setattr(function_info_class, attribute_name,
                _make_measure_property(measure_name))
    setattr(function_info_class, "_halstead_patched", True)


# Best-effort patch of the imported class for the common single-import case;
# the extension also patches the concrete class lazily while streaming.
ensure_function_info_patched(FunctionInfo)


class LizardExtension(object):  # pylint: disable=too-few-public-methods

    FUNCTION_INFO = {
        "halstead_volume": {"caption": " H-volume "},
        "halstead_difficulty": {"caption": " H-diff "},
        "halstead_effort": {"caption": " H-effort "},
    }

    @staticmethod
    def cross_file_process(fileinfos):
        # Runs in the parent process after every file has been analyzed (and,
        # under multiprocessing, after the results have been unpickled).  The
        # per-token patch in ``__call__`` only ran in the worker, so the class
        # backing the collected results may still be missing the Halstead
        # properties here; patch it before the output scheme reads them.
        for fileinfo in fileinfos:
            for function in getattr(fileinfo, "function_list", None) or []:
                ensure_function_info_patched(type(function))
            yield fileinfo

    @staticmethod
    def __call__(tokens, reader):
        # Kept free of per-call state on the extension object: this generator
        # runs once per file and the same instance is shared across threads.
        classifier = get_classifier(reader)
        operator = HalsteadClassifier.OPERATOR
        operand = HalsteadClassifier.OPERAND
        for token in tokens:
            function = reader.context.current_function
            operators = getattr(function, "_halstead_operators", None)
            if operators is None:
                # Patch the concrete class actually in use (which may be
                # ``__main__.FunctionInfo`` under ``python -m lizard`` or a
                # worker's copy under multiprocessing).  Idempotent and cheap.
                ensure_function_info_patched(type(function))
                operators = function._halstead_operators = Counter()
                function._halstead_operands = Counter()
            kind = classifier.classify(token)
            if kind is operator:
                operators[token] += 1
            elif kind is operand:
                function._halstead_operands[token] += 1
            yield token
