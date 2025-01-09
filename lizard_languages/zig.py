"""
Language parser for Zig
"""

from __future__ import annotations

from .clike import CCppCommentsMixin
from .code_reader import CodeReader
from .golike import GoLikeStates


class ZigReader(CodeReader, CCppCommentsMixin):
    ext = ["zig"]
    language_names = ["zig"]
    _conditions = {"if", "for", "while", "and", "or", "orelse", "try", "catch", "=>"}

    def __init__(self, context):
        super().__init__(context)
        self.parallel_states = [ZigStates(context)]


class ZigStates(GoLikeStates):
    FUNC_KEYWORD = "fn"

    _type_definition = GoLikeStates._state_global
