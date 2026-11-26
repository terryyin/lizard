"""
Language parser for PL/SQL (Oracle's Procedural Language extension to SQL)

This module implements complexity analysis for PL/SQL code, supporting:
- Procedures, Functions, and Triggers
- Package Bodies (not specifications - they only contain signatures)
- Nested procedures and functions
- Anonymous blocks with nested functions (blocks themselves aren't counted)
- Control structures: IF/ELSIF/ELSE, CASE/WHEN, LOOP/WHILE/FOR
- Exception handlers
- Cursor declarations and cursor FOR loops

Design Decisions:
- Keyword casing, compound END keywords and EXIT WHEN: handled by token
  preprocessing in plsql_keywords.

- CONTINUE WHEN: Similar to EXIT WHEN, the WHEN is counted as it does create
  a branch in the loop execution.

- GOTO: Does not add to cyclomatic complexity as it's just an unconditional
  jump, not a decision point.

- Standalone LOOP: Adds +1 complexity as it creates a repeating path.

- FOR/WHILE LOOP: The FOR/WHILE keyword adds complexity; the following LOOP
  keyword is part of the same construct and doesn't add additional complexity.

- Parameter Counting: Currently counts all non-whitespace tokens and commas
  in parameter lists. This approach works but differs from some other language
  implementations.
"""

from .code_reader import CodeReader, CodeStateMachine
from .clike import CCppCommentsMixin
from .plsql_declarations import PLSQLDeclarationStates
from .plsql_keywords import case_insensitive_conditions, merge_compound_keywords


class PLSQLReader(CodeReader, CCppCommentsMixin):
    """
    Reader for PL/SQL language supporting procedures, functions, packages,
    and core control structures.
    """

    ext = ["sql", "pks", "pkb", "pls", "plb", "pck"]
    language_names = ["plsql", "pl/sql"]

    # Separated condition categories
    # Note: 'loop' is NOT in this set because LOOP has special handling:
    # - standalone LOOP adds +1
    # - LOOP after WHILE/FOR should not add (it's part of the compound statement)
    _control_flow_keywords = {"if", "elsif", "when", "while", "for"}
    _logical_operators = {"and", "or"}
    _case_keywords = set()  # PL/SQL uses 'when' in case expressions
    _ternary_operators = set()

    def __init__(self, context):
        super(PLSQLReader, self).__init__(context)
        self.parallel_states = [PLSQLStates(context)]
        # PL/SQL is case-insensitive; keyword casing lives in plsql_keywords
        self.conditions = case_insensitive_conditions(self.conditions)

    def preprocess(self, tokens):
        """Apply PL/SQL keyword preprocessing (see plsql_keywords)."""
        return merge_compound_keywords(tokens)

    @staticmethod
    def generate_tokens(source_code, addition="", token_class=None):
        """
        Generate tokens for PL/SQL code.
        PL/SQL uses:
        - Single-line comments: --
        - Multi-line comments: /* */
        - String literals: 'text' (with '' for escaping)
        - Alternative-quoted literals: q'[text]' or nq'#text#', any case
        - Assignment operator: :=
        """
        # Add PL/SQL-specific patterns
        addition = (
            r"|[nN]?[qQ]'(?:\[.*?\]'|\{.*?\}'|\(.*?\)'|<.*?>'|"
            r"(?P<sql_quote>[^\s\[({<]).*?(?P=sql_quote)')"
            r"|--[^\n]*" + addition)  # Single-line comment starting with --
        return CodeReader.generate_tokens(source_code, addition, token_class)

    def get_comment_from_token(self, token):
        """
        Override to recognize PL/SQL's -- line comments in addition to /* */ block comments.
        PL/SQL uses -- for single-line comments (like SQL standard).

        Note: This method correctly identifies -- comments, but due to a limitation in
        the NLOC calculation, these comments may still be counted in NLOC.
        """
        if token.startswith("--"):
            return token  # Return full comment token (like Lua does)
        # Delegate to parent for /* */ and // comments
        return super().get_comment_from_token(token)


class PLSQLStates(PLSQLDeclarationStates, CodeStateMachine):
    """
    State machine for parsing PL/SQL code structure.
    """

    def __init__(self, context):
        super(PLSQLStates, self).__init__(context)
        self.last_control_keyword = None  # Track FOR/WHILE to avoid counting their LOOP
        self.declaring_nested_function = (
            False  # Track if we're declaring a nested function
        )

    def _state_body(self, token):
        """
        Process function/procedure body.
        Track control structures for cyclomatic complexity.
        Manually track BEGIN/END blocks.
        """
        token_lower = token.lower()
        token_upper = token.upper()

        # Check for merged compound keywords like "END_IF", "END_LOOP", etc.
        # These are created by the preprocessor
        if token_lower.startswith("end_"):
            # This is a compound END keyword, reset tracking
            self.last_control_keyword = None
            return

        # Handle nested procedure/function declarations
        if token_lower == "procedure":
            self.next(self._procedure_name)
            return
        elif token_lower == "function":
            self.next(self._function_name)
            return

        # Track FOR and WHILE to know when LOOP follows them
        if token_upper in ("FOR", "WHILE"):
            self.last_control_keyword = token_upper

        # Handle LOOP keyword manually
        # - Standalone LOOP adds +1 complexity
        # - LOOP after FOR/WHILE does not add complexity (already counted for FOR/WHILE)
        elif token_upper == "LOOP":
            if self.last_control_keyword not in ("FOR", "WHILE"):
                # This is a standalone LOOP, add complexity
                self.context.add_condition()
            # Reset tracking after processing LOOP
            self.last_control_keyword = None

        # PL/SQL uses BEGIN/END instead of {}
        if token_lower == "begin":
            self.br_count += 1
            self.context.add_bare_nesting()
        elif token_lower == "end":
            # This is a standalone END (for BEGIN/END block)
            self.br_count -= 1
            if self.br_count == 0:
                # This END closes the function/procedure
                # Check if we have a parent function BEFORE ending (stack gets popped)
                has_parent = len(self.context.stacked_functions) > 0
                self.context.end_of_function()
                # Return to appropriate state based on whether this was nested
                if has_parent:
                    # Return to parent function's declaration section
                    self.next(self._state_before_begin)
                else:
                    # No parent function, return to global
                    self.next(self._state_global)
                return
            else:
                self.context.pop_nesting()

        # Note: Basic conditions (if, elsif, when, while, for, and, or)
        # are automatically counted by the condition_counter processor
        # based on the _conditions set in the Reader class.
