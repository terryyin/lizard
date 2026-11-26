"""Go-specific nesting events and statement boundaries."""


def go_nesting_tokens(tokens, reader):
    """Track Go statement and composite literal roles before ND reads a token."""
    context = reader.context
    block_starts = getattr(reader, 'block_starts', ('{',))
    block_ends = getattr(reader, 'block_ends', ('}',))
    for token in tokens:
        function = context.current_function
        function.go_starts_if = token == 'if'
        if token in block_starts:
            function.go_composite_literal_brace = getattr(token, 'is_composite_literal_brace', False)
        elif token in block_ends:
            if function.go_if_levels is None and function.condition_depth == 0:
                # A case or a statement can end at a brace without a semicolon.
                end_go_statement(context)
        elif token in ('=', ':='):
            if function.go_if_levels is not None and function.condition_depth == 0:
                function.go_initializer = True
        elif (token in ('&&', '||') and function.go_if_levels is None
              and (not function.in_condition or not function.logical_operator_added)):
            function.go_statement_logical_levels += 1
        yield token


def end_go_statement(context):
    """Release a Go statement while preserving its if initializer level."""
    function = context.current_function
    function.go_statement_logical_levels = 0
    function.go_initializer = False
    # An explicit semicolon can separate a Go if initializer and test.
    keep_levels = function.go_if_levels or 0
    if keep_levels:
        function.logical_operator_added = False
    levels = function.levels_awaiting_body
    function.levels_awaiting_body = keep_levels
    return context.add_nd_condition(keep_levels - levels)
