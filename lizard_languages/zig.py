"""
Language parser for Zig
"""

from __future__ import annotations

from .clike import CCppCommentsMixin
from .code_reader import CodeReader
from .golike import GoLikeStates


class ZigReader(CodeReader, CCppCommentsMixin):
    # pylint: disable=R0903

    ext = ["zig"]
    language_names = ["zig"]
    _conditions = {"if", "for", "while", "and", "or", "orelse", "try", "catch", "=>"}

    def __init__(self, context):
        super(ZigReader, self).__init__(context)
        self.parallel_states = [ZigStates(context)]


class ZigStates(GoLikeStates):  # pylint: disable=R0903
    FUNC_KEYWORD = "fn"

    _type_definition = GoLikeStates._state_global
