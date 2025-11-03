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
    
    # Separated condition categories
    _control_flow_keywords = {"if", "for", "while", "try", "catch"}
    _logical_operators = {"and", "or", "orelse"}  # orelse is Zig's null coalescing
    _case_keywords = set()
    # Note: '=>' is for error union and switch cases in Zig
    _ternary_operators = {"=>"}

    def __init__(self, context):
        super().__init__(context)
        self.parallel_states = [ZigStates(context)]


class ZigStates(GoLikeStates):
    FUNC_KEYWORD = "fn"

    _type_definition = GoLikeStates._state_global
