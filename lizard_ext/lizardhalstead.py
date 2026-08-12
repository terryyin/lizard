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
  language (``True``/``False``/``None``/``...`` in Python, ``true``/``false``/``null``
  and friends elsewhere).  Python string prefixes (``f``, ``b``, ``r``, ...) that
  lizard emits as a separate token immediately before a quoted string are
  skipped so only the string body counts as an operand.
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

from collections import Counter

from lizard import FunctionInfo

from .halstead_classifier import (
    HalsteadClassifier,
    PythonHalsteadClassifier,
    get_classifier,
)
from .halstead_metrics import HalsteadMetrics

# Re-export for callers that import from this extension module.
__all__ = (
    'HalsteadClassifier',
    'HalsteadMetrics',
    'LizardExtension',
    'PythonHalsteadClassifier',
    'ensure_function_info_patched',
    'get_classifier',
)


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
        # Potential Python string prefixes are classified one token late so we
        # can see whether a quoted string follows, without pulling ahead on the
        # shared token stream (which would desync upstream line/token counters
        # from the language reader).
        deferred = None  # (function, token)

        def apply_token(function, token, next_token):
            operators = getattr(function, "_halstead_operators", None)
            if operators is None:
                # Patch the concrete class actually in use (which may be
                # ``__main__.FunctionInfo`` under ``python -m lizard`` or a
                # worker's copy under multiprocessing).  Idempotent and cheap.
                ensure_function_info_patched(type(function))
                operators = function._halstead_operators = Counter()
                function._halstead_operands = Counter()
            kind = classifier.classify_with_next(token, next_token)
            if kind is operator:
                operators[token] += 1
            elif kind is operand:
                function._halstead_operands[token] += 1

        for token in tokens:
            function = reader.context.current_function
            if deferred is not None:
                deferred_function, deferred_token = deferred
                deferred = None
                apply_token(deferred_function, deferred_token, token)
            if classifier.needs_next_token(token):
                deferred = (function, token)
            else:
                apply_token(function, token, None)
            yield token
        if deferred is not None:
            deferred_function, deferred_token = deferred
            apply_token(deferred_function, deferred_token, None)
