"""
This is an extension of lizard that calculates Cognitive Complexity
for every function.

Cognitive Complexity is a metric designed to measure the relative difficulty of
understanding code's control flow. Unlike Cyclomatic Complexity, which is based
on a mathematical model of the number of linearly independent paths through code,
Cognitive Complexity uses human judgment to assess the mental effort required to
understand control flow structures.

This implementation follows the Cognitive Complexity specification version 1.2
by SonarSource (19 April 2017). For details, see cognitive_complexity_theory.rst
in the repository root.
"""

DEFAULT_COGC_THRESHOLD = 15


class LizardExtension(object):  # pylint: disable=R0903
    """Cognitive Complexity extension for Lizard."""

    FUNCTION_INFO = {
        "cognitive_complexity": {
            "caption": " CogC  ",
            "average_caption": " Avg.CogC "
        }
    }

    @staticmethod
    def set_args(parser):
        parser.add_argument(
            "-G", "--CogC",
            help='''Threshold for Cognitive Complexity warning.
            The default value is %d.
            Functions with CogC bigger than it will generate warning
            ''' % DEFAULT_COGC_THRESHOLD,
            type=int,
            dest="CogC",
            default=DEFAULT_COGC_THRESHOLD)
        parser.add_argument(
            "--cogc-lines",
            help='''Show line-by-line breakdown of Cognitive Complexity increments
            for functions that exceed thresholds. Only works with default text output
            (not HTML/XML/CSV). Useful for debugging and understanding what contributes
            to complexity.
            ''',
            action="store_true",
            dest="cogc_lines",
            default=False)

    def _create_initial_state(self, reader):
        """Create initial state dictionary for token processing."""
        is_erlang = hasattr(reader, 'language_names') and 'erlang' in reader.language_names
        is_python = hasattr(reader, 'language_names') and 'python' in reader.language_names

        return {
            'in_switch': False,
            'switch_nesting': 0,
            'after_else': False,
            'in_do_while': False,
            'after_break_continue': False,
            'saw_question_mark': False,
            'in_exception_block': False,
            'after_loop_keyword': False,
            'is_erlang': is_erlang,
            'is_python': is_python,
            'erlang_if_case_depth': 0,
            'after_semicolon_in_if_case': False,
            'after_dash_in_erlang': False,
            'nesting_stack': [],
            'brace_depth': 0,
            'pending_nesting_increase': False,
            'pending_try_block': False,
            'try_block_stack': [],
            'bracket_depth': 0,  # Track [], (), {} depth for Python comprehensions
        }

    def _handle_binary_logical_operator(self, context, token):
        """Handle binary logical operators: &&, ||, and, or, etc.

        Fundamental increment - no nesting multiplier (spec line 209-216).
        Only increment on first operator or when switching operator types.
        """
        # Normalize operator for comparison
        # and/&&/.and./andalso are equivalent, or/||/.or./orelse are equivalent
        normalized = 'and' if token.lower() in ('&&', 'and', '.and.', 'andalso') else 'or'

        # Only increment on first operator or when switching operator types
        if context.cogc_last_operator != normalized:
            # This is a fundamental increment: +1 without nesting multiplier
            # Record manually since we're not using add_cognitive_complexity
            context.current_function.cognitive_complexity_increments.append({
                'line': context.current_line,
                'increment': 1,
                'base_increment': 1,
                'nesting': 0,  # Logical operators don't get nesting multiplier
                'nesting_from_stack': 0,
                'nesting_from_cogc': 0,
                'reason': f"{normalized} operator",
                'token': token
            })
            context.current_function.cognitive_complexity += 1
            context.cogc_last_operator = normalized

    def _handle_break_continue_label(self, context, token):
        """Check if break/continue is followed by a label.

        Returns True if this token was processed, False otherwise.
        Labeled jumps add +1 (fundamental increment).
        """
        # If token is not a statement terminator, it's a label
        if token not in (';', '}', '\\n'):
            # This is a labeled jump: add +1 (fundamental increment)
            # Record the increment for line-by-line reporting
            context.current_function.cognitive_complexity_increments.append({
                'line': context.current_line,
                'increment': 1,
                'base_increment': 1,
                'nesting': 0,
                'nesting_from_stack': 0,
                'nesting_from_cogc': 0,
                'reason': f'labeled jump to {token}',
                'token': token
            })
            context.current_function.cognitive_complexity += 1
        return True  # Processed

    def _handle_opening_brace(self, context, brace_depth, pending_try_block,
                               pending_nesting_increase, try_block_stack, nesting_stack):
        """Handle opening brace: try blocks, nesting, lambdas.

        Returns updated values for: brace_depth, pending_try_block, pending_nesting_increase
        """
        brace_depth += 1

        # Check if this is a try block (excluded from cognitive nesting)
        if pending_try_block:
            context.cogc_excluded_nesting += 1
            try_block_stack.append(True)
            pending_try_block = False
        else:
            try_block_stack.append(False)

        # Check if this brace should increase nesting
        # Both structural keywords (if, for, etc.) and lambdas increase nesting
        if pending_nesting_increase or context.pending_lambda_nesting:
            context.increase_cogc_nesting()
            nesting_stack.append(True)  # This brace increases nesting
            pending_nesting_increase = False
            context.pending_lambda_nesting = False
        else:
            nesting_stack.append(False)  # This brace doesn't increase nesting

        # Reset flags
        context.pending_structural_nesting = False  # Reset to avoid double-nesting in clike.py
        context.cogc_last_operator = None  # Reset binary logical operator tracking

        return brace_depth, pending_try_block, pending_nesting_increase

    def _handle_closing_brace(self, context, brace_depth, in_switch, switch_nesting,
                               try_block_stack, nesting_stack):
        """Handle closing brace or }nosync.

        Returns updated values for: brace_depth, in_switch
        """
        # Check if this closes a try block
        if try_block_stack:
            is_try_block = try_block_stack.pop()
            if is_try_block:
                context.cogc_excluded_nesting -= 1

        if nesting_stack:
            increased_nesting = nesting_stack.pop()
            if increased_nesting:
                context.decrease_cogc_nesting()

        brace_depth -= 1

        # Check if we exited a switch
        if in_switch and brace_depth == switch_nesting:
            in_switch = False

        return brace_depth, in_switch

    def _handle_end_keyword(self, context, is_erlang, erlang_if_case_depth):
        """Handle 'end' keyword for Lua/Ruby/Erlang.

        Note: Fortran also has 'end' tokens, but Fortran calls pop_nesting() which handles
        CogC stack automatically. We only handle structural nesting (True) here.

        Returns updated erlang_if_case_depth.
        """
        # Erlang: track exiting if/case block
        if is_erlang and erlang_if_case_depth > 0:
            erlang_if_case_depth -= 1

        # Only pop if we added structural nesting for a control structure
        # Non-structural nesting (False) is handled by language state machine's pop_nesting()
        if context.cogc_nesting_stack and context.cogc_nesting_stack[-1]:
            # This is a structural nesting level (if/while/for/case), pop it
            context.decrease_cogc_nesting()

        # Reset binary operator tracking (new statement/block)
        context.cogc_last_operator = None

        return erlang_if_case_depth

    def _handle_structural_keyword(self, context, token, after_else, in_do_while,
                                    erlang_if_case_depth, is_erlang, pending_nesting_increase,
                                    inside_brackets=False):
        """Handle structural keywords: if, for, while, foreach, repeat.

        These keywords increase both complexity and nesting level.
        Special cases:
        - Don't count 'if' after 'else' (else-if is one increment total)
        - Don't count 'while' at end of do-while loop
        - Track for/while to distinguish C do-while from Lua/Ruby do-after-loop
        - Track Erlang if/case depth for clause separator handling
        - Python: Don't set pending_structural_nesting when inside brackets (comprehensions)

        Returns updated values for: after_else, in_do_while, erlang_if_case_depth,
                                     after_loop_keyword, pending_nesting_increase
        """
        after_loop_keyword = False

        # Don't count 'if' after 'else' (else-if is one increment)
        if not (token == 'if' and after_else):
            # Don't count 'while' if it's the end of a do-while
            if not (token == 'while' and in_do_while):
                # Provide detailed reason for the increment
                if inside_brackets:
                    reason = f"{token} in comprehension"
                else:
                    reason = f"{token} statement"
                context.add_cognitive_complexity(inc=1, reason=reason, token=token)
                pending_nesting_increase = True
                # For brace-less languages (Python), increase nesting immediately
                # The next add_bare_nesting() will mark it properly
                # BUT: Skip if inside brackets (comprehensions don't create structural nesting)
                if not inside_brackets:
                    context.pending_structural_nesting = True
        else:
            # This is the 'if' part of 'else if', still needs to set nesting
            pending_nesting_increase = True
            if not inside_brackets:
                context.pending_structural_nesting = True

        after_else = False

        if token == 'while' and in_do_while:
            in_do_while = False

        # Track if we saw 'for' or 'while' (for Lua/Ruby where 'do' follows)
        if token in ('for', 'while'):
            after_loop_keyword = True

        # Erlang: track if we entered an if or case block
        if is_erlang and token in ('if', 'case'):
            erlang_if_case_depth += 1

        return after_else, in_do_while, erlang_if_case_depth, after_loop_keyword, pending_nesting_increase

    def _handle_do_keyword(self, context, after_loop_keyword, in_do_while, pending_nesting_increase):
        """Handle 'do' keyword - distinguish C do-while from Lua/Ruby do-after-loop.

        In Lua/Ruby, 'do' follows 'for'/'while' and doesn't add complexity (already counted).
        In C/C++/Java, 'do' starts a do-while loop and should be counted.

        Returns updated values for: after_loop_keyword, in_do_while, pending_nesting_increase
        """
        # In Lua/Ruby, 'do' follows 'for' or 'while' and shouldn't be counted separately
        # In C/C++/Java, 'do' starts a do-while loop and should be counted
        if not after_loop_keyword:
            # C-style do-while
            context.add_cognitive_complexity(inc=1, reason="do-while loop", token="do")
            pending_nesting_increase = True
            in_do_while = True
        else:
            # Lua/Ruby-style: do follows for/while, add nesting for the block
            if context.pending_structural_nesting:
                context.add_bare_nesting()

        after_loop_keyword = False  # Reset the flag

        # Reset binary operator tracking (new block starting)
        context.cogc_last_operator = None

        return after_loop_keyword, in_do_while, pending_nesting_increase

    def _handle_erlang_clause_separator(self, context, token, erlang_if_case_depth,
                                         after_semicolon_in_if_case, after_dash_in_erlang):
        """Handle Erlang clause separators in if/case statements.

        Erlang uses ';' to separate clauses and '->' to start clause bodies.
        Each clause after the first (pattern: '; ... ->') counts like an elsif.

        Returns tuple of (handled, after_semicolon_in_if_case, after_dash_in_erlang)
        where handled=True if this method processed the token.
        """
        # Erlang: handle semicolon in if/case statements
        if token == ';' and erlang_if_case_depth > 0:
            # Mark that we saw a semicolon inside an if/case block
            after_semicolon_in_if_case = True
            context.cogc_last_operator = None
            return True, after_semicolon_in_if_case, after_dash_in_erlang

        # Erlang: track '-' which might be part of '->' arrow
        elif token == '-':
            after_dash_in_erlang = True
            return True, after_semicolon_in_if_case, after_dash_in_erlang

        # Erlang: '>' after '-' forms '->' (arrow operator)
        # When this appears after ';' in if/case, it indicates a new clause (like elsif)
        elif token == '>' and after_dash_in_erlang:
            after_dash_in_erlang = False
            if after_semicolon_in_if_case:
                # This is a new clause in an if/case statement
                # NOTE: Due to Erlang parser limitations, this increment may go to the wrong function
                # when ';' appears in if/case blocks (parser treats ';' as function terminator)
                context.add_cognitive_complexity(inc=1, reason="case clause", token=";")
                after_semicolon_in_if_case = False
            context.cogc_last_operator = None
            return True, after_semicolon_in_if_case, after_dash_in_erlang

        # Not handled by this method
        return False, after_semicolon_in_if_case, after_dash_in_erlang

    def __call__(self, tokens, reader):
        '''
        Calculate Cognitive Complexity according to the specification.
        Three basic rules:
        1. Ignore shorthand structures (methods, null-coalescing)
        2. Increment for breaks in linear flow
        3. Increment for nesting
        '''
        context = reader.context
        prev_token = None

        # Initialize state variables
        state = self._create_initial_state(reader)
        in_switch = state['in_switch']
        switch_nesting = state['switch_nesting']
        after_else = state['after_else']
        in_do_while = state['in_do_while']
        after_break_continue = state['after_break_continue']
        saw_question_mark = state['saw_question_mark']
        in_exception_block = state['in_exception_block']
        after_loop_keyword = state['after_loop_keyword']
        is_erlang = state['is_erlang']
        is_python = state['is_python']
        erlang_if_case_depth = state['erlang_if_case_depth']
        after_semicolon_in_if_case = state['after_semicolon_in_if_case']
        after_dash_in_erlang = state['after_dash_in_erlang']
        nesting_stack = state['nesting_stack']
        brace_depth = state['brace_depth']
        pending_nesting_increase = state['pending_nesting_increase']
        pending_try_block = state['pending_try_block']
        try_block_stack = state['try_block_stack']
        bracket_depth = state['bracket_depth']

        # Structural keywords that increase both complexity and nesting
        # Includes C preprocessor directives like #if, #ifdef
        # repeat is for Lua's repeat...until loop
        structural_keywords = {'if', 'for', 'while', 'foreach', 'repeat', '#if', '#ifdef', '#elif'}
        # 'case' is special: it's a structural keyword in Erlang and ST, but not in C/C++/Java (where it's a switch label)
        if hasattr(reader, 'language_names') and ('erlang' in reader.language_names or 'st' in reader.language_names):
            structural_keywords.add('case')
        # catch/except and try (except is Python, catch is C++/Java/C#/JavaScript)
        catch_keywords = {'catch', 'except'}
        # Keywords that create blocks but don't increase cognitive nesting
        try_keywords = {'try', 'synchronized'}
        # Labeled jumps
        jump_keywords = {'goto'}
        # PL/SQL exception block keyword
        exception_keywords = {'exception'}

        for token in tokens:
            # Python: Track bracket depth for comprehensions
            # When inside [], (), or {}, control flow keywords should not set pending_structural_nesting
            if is_python:
                if token in ('[', '(', '{'):
                    bracket_depth += 1
                elif token in (']', ')', '}'):
                    bracket_depth = max(0, bracket_depth - 1)
                    # Clear pending_structural_nesting when closing brackets
                    # This prevents leakage from comprehensions to subsequent blocks
                    context.pending_structural_nesting = False
                    # Reset binary logical operator tracking (exiting comprehension/expression)
                    context.cogc_last_operator = None

            # Check if previous token was '?' and current is not '.' or '?'
            # If so, the '?' was a ternary operator
            # (Ignore null-coalescing: ?. and ??)
            if saw_question_mark:
                if token not in ('.', '?'):
                    # The '?' was a ternary operator, count it
                    context.add_cognitive_complexity(inc=1, reason="ternary operator", token="?")
                    pending_nesting_increase = True
                # Reset the flag regardless
                saw_question_mark = False

            # Track opening braces
            if token == '{':
                brace_depth, pending_try_block, pending_nesting_increase = \
                    self._handle_opening_brace(context, brace_depth, pending_try_block,
                                               pending_nesting_increase, try_block_stack, nesting_stack)
                after_else = False  # Reset after_else when we see a brace
                after_loop_keyword = False  # Reset for/while tracking (C++ uses {, not do)

            # Track closing braces (including PL/SQL's }nosync for END IF/LOOP/CASE)
            elif token == '}' or token == '}nosync':
                brace_depth, in_switch = \
                    self._handle_closing_brace(context, brace_depth, in_switch, switch_nesting,
                                               try_block_stack, nesting_stack)
                after_else = False  # Reset after_else

            # Track closing 'end' keyword (for Lua/Ruby/Erlang)
            elif token == 'end':
                erlang_if_case_depth = self._handle_end_keyword(context, is_erlang, erlang_if_case_depth)
                after_else = False  # Reset after_else

            # C/C++ preprocessor directives: #if, #ifdef, #elif
            # These are counted as structural increments like regular 'if'
            # Unlike regular 'if', preprocessor directives don't have braces, so we increase nesting immediately
            elif token.startswith('#if') or token.startswith('#elif'):
                context.add_cognitive_complexity(inc=1, reason=f"{token} preprocessor directive", token=token)
                # Preprocessor directives don't use braces, so increase nesting immediately
                context.increase_cogc_nesting()

            # Structural increments: if, for, while, foreach
            elif token in structural_keywords:
                # Python: Pass bracket_depth to indicate if we're inside comprehensions
                inside_brackets = is_python and bracket_depth > 0
                # DEBUG
                #if is_python:
                #    print(f"DEBUG: token={token}, bracket_depth={bracket_depth}, inside_brackets={inside_brackets}")
                after_else, in_do_while, erlang_if_case_depth, after_loop_keyword, pending_nesting_increase = \
                    self._handle_structural_keyword(context, token, after_else, in_do_while,
                                                     erlang_if_case_depth, is_erlang, pending_nesting_increase,
                                                     inside_brackets)

            # do-while loop (C/C++/Java) or do keyword after for/while (Lua/Ruby)
            elif token == 'do':
                after_loop_keyword, in_do_while, pending_nesting_increase = \
                    self._handle_do_keyword(context, after_loop_keyword, in_do_while, pending_nesting_increase)

            # else and else-if
            # These are HYBRID increments:
            # - They ADD to complexity (+1)
            # - They DO increase nesting level for things inside them
            # - But they DON'T get nesting penalty themselves (mental cost already paid)
            # NOTE: 'else' inside a switch/match statement does NOT add complexity
            # (the switch itself already counted as +1)
            elif token == 'else' and not in_switch:
                # Fundamental increment: +1 without nesting multiplier
                # Record the increment for line-by-line reporting
                context.current_function.cognitive_complexity_increments.append({
                    'line': context.current_line,
                    'increment': 1,
                    'base_increment': 1,
                    'nesting': 0,
                    'nesting_from_stack': 0,
                    'nesting_from_cogc': 0,
                    'reason': 'else statement',
                    'token': token
                })
                context.current_function.cognitive_complexity += 1
                pending_nesting_increase = True
                after_else = True

            # elif/elseif/elsif/else if (some languages use elsif like PL/SQL, Ruby; Fortran uses 'else if')
            elif token in ('elif', 'elseif', 'elsif', 'else if'):
                # Fundamental increment: +1 without nesting multiplier
                # Record the increment for line-by-line reporting
                context.current_function.cognitive_complexity_increments.append({
                    'line': context.current_line,
                    'increment': 1,
                    'base_increment': 1,
                    'nesting': 0,
                    'nesting_from_stack': 0,
                    'nesting_from_cogc': 0,
                    'reason': 'elif statement',
                    'token': token
                })
                context.current_function.cognitive_complexity += 1
                pending_nesting_increase = True

            # catch clause (or PL/SQL EXCEPTION WHEN)
            elif token in catch_keywords or (in_exception_block and token == 'when'):
                reason = f"{token} clause"
                context.add_cognitive_complexity(inc=1, reason=reason, token=token)
                pending_nesting_increase = True

            # PL/SQL EXCEPTION keyword - following WHEN clauses act like catch
            elif token in exception_keywords:
                in_exception_block = True

            # try doesn't add complexity, and try blocks don't increase nesting (spec line 234)
            elif token in try_keywords:
                pending_try_block = True

            # Switch/match statement - single increment for entire switch
            # (switch in C/Java/JavaScript, match in Rust/Scala)
            elif token in ('switch', 'match'):
                context.add_cognitive_complexity(inc=1, reason=f"{token} statement", token=token)
                in_switch = True
                switch_nesting = brace_depth
                pending_nesting_increase = True

            # Binary logical operators: &&, ||, and, or, etc.
            # Note: || is string concatenation in SQL/PL/SQL, not a logical operator
            elif token.lower() in ('&&', '||', 'and', 'or', '.and.', '.or.', 'andalso', 'orelse'):
                # Skip || in SQL/PL/SQL languages (it's concatenation, not OR)
                is_sql_language = hasattr(reader, 'language_names') and any(
                    lang in reader.language_names for lang in ['plsql', 'pl/sql', 'sql']
                )
                if token == '||' and is_sql_language:
                    pass  # Skip - this is concatenation, not logical OR
                else:
                    self._handle_binary_logical_operator(context, token)

            # Ternary operator: condition ? true_value : false_value
            # The '?' acts like an 'if' statement
            # But ignore null-coalescing operators: ?. and ??
            elif token == '?':
                # Check if this is null-coalescing (?. or ??) or ternary
                # If previous token is also '?', this is ?? (null-coalescing, handled as single token in C#)
                # Note: We need to wait to see if next token is '.' to determine if this is ?.
                # For now, mark that we saw a '?' and check on the next iteration.
                # We use a flag to defer the decision.
                saw_question_mark = True

            # Labeled jumps: goto LABEL, break LABEL, continue LABEL
            # goto always counts, break/continue only count if followed by a label
            elif token in jump_keywords:
                # goto always adds +1 (fundamental increment without nesting multiplier)
                # Record the increment for line-by-line reporting
                context.current_function.cognitive_complexity_increments.append({
                    'line': context.current_line,
                    'increment': 1,
                    'base_increment': 1,
                    'nesting': 0,
                    'nesting_from_stack': 0,
                    'nesting_from_cogc': 0,
                    'reason': f'{token} statement',
                    'token': token
                })
                context.current_function.cognitive_complexity += 1
            elif token in ('break', 'continue'):
                # Mark that we saw break/continue, will check next token to see if it's a label
                after_break_continue = True
            # Check if current token is a label after break/continue
            elif after_break_continue:
                self._handle_break_continue_label(context, token)
                after_break_continue = False


            # Erlang: handle clause separators in if/case statements
            # Each clause after the first (separated by ';') counts like an elsif
            elif is_erlang:
                handled, after_semicolon_in_if_case, after_dash_in_erlang = \
                    self._handle_erlang_clause_separator(context, token, erlang_if_case_depth,
                                                          after_semicolon_in_if_case, after_dash_in_erlang)
                # If not handled, fall through to the final elif (operator tracking reset)
                if not handled:
                    # Reset operator tracking on statement boundaries
                    if token in (';', '\\n', '{'):
                        context.cogc_last_operator = None

            # Reset operator tracking on statement boundaries
            elif token in (';', '\\n', '{'):
                context.cogc_last_operator = None

            prev_token = token
            yield token


def _cogc_bare_nesting_hook(context):
    """Hook for add_bare_nesting() to handle CogC structural nesting."""
    # Check if this is structural nesting (for brace-less languages like Lua/Ruby/Python)
    # If pending_structural_nesting is True, this block increases CogC nesting
    if context.pending_structural_nesting:
        context.increase_cogc_nesting()
        context.pending_structural_nesting = False
    else:
        # Mark that this nesting level does NOT increase cognitive complexity
        # (Only structural control flow like if/for/while increases CogC nesting)
        context.cogc_nesting_stack.append(False)


def _cogc_pop_nesting_hook(context):
    """Hook for pop_nesting() to handle CogC nesting stack."""
    # Check if this nesting level corresponds to cognitive complexity nesting
    if context.cogc_nesting_stack and context.cogc_nesting_stack[-1]:
        context.decrease_cogc_nesting()
    elif context.cogc_nesting_stack:
        context.cogc_nesting_stack.pop()  # Pop the False marker


class CogCFileInfoAddition(object):
    """Methods to add to FileInfoBuilder for Cognitive Complexity tracking."""

    def add_cognitive_complexity(self, inc=1, reason="", token=""):
        '''Add to cognitive complexity with current nesting level

        Args:
            inc: Base increment (usually 1)
            reason: Human-readable reason for the increment (e.g., "if statement", "for loop")
            token: The token that caused the increment
        '''
        # Calculate nesting using both tracking methods:
        # 1. current_nesting_level: Bracket-based nesting from NestingStack (used by C/C++/Java)
        # 2. cogc_nesting_level: Control-flow based nesting (used for all languages, especially JavaScript)
        #
        # For languages with NestingStack (C/C++/Java):
        #   - Use current_nesting_level minus initial offset
        # For languages without NestingStack (JavaScript):
        #   - Use cogc_nesting_level plus any initial cogc nesting (e.g., from preprocessor directives)
        #
        # We use the maximum of both to support both approaches
        nesting_from_stack = max(0, self.current_nesting_level - self.current_function.initial_nesting_level - 1)
        nesting_from_cogc = self.cogc_nesting_level + self.current_function.initial_cogc_nesting_level
        nesting = max(nesting_from_stack, nesting_from_cogc) - self.cogc_excluded_nesting
        nesting = max(0, nesting)
        total_inc = inc + nesting

        # Record the increment for line-by-line reporting
        self.current_function.cognitive_complexity_increments.append({
            'line': self.current_line,
            'increment': total_inc,
            'base_increment': inc,
            'nesting': nesting,
            'nesting_from_stack': nesting_from_stack,
            'nesting_from_cogc': nesting_from_cogc,
            'reason': reason,
            'token': token
        })

        self.current_function.cognitive_complexity += total_inc

    def increase_cogc_nesting(self):
        '''Increase cognitive complexity nesting level for structural control flow'''
        self.cogc_nesting_level += 1
        # Always add a True marker to track this cognitive nesting level
        self.cogc_nesting_stack.append(True)

    def decrease_cogc_nesting(self):
        '''Decrease cognitive complexity nesting level'''
        if self.cogc_nesting_level > 0:
            self.cogc_nesting_level -= 1
        if self.cogc_nesting_stack:
            self.cogc_nesting_stack.pop()

    def reset_cogc_nesting(self):
        '''Reset cognitive complexity nesting level (e.g., at function boundary)'''
        self.cogc_nesting_level = 0
        self.cogc_last_operator = None
        self.cogc_nesting_stack = []

    def enter_lambda(self):
        '''Mark that a lambda/anonymous function is being entered.
        This increases nesting for code inside the lambda, but does NOT create a new FunctionInfo.
        The complexity of the lambda body is added to the current (parent) function.
        '''
        self.pending_lambda_nesting = True
        self.lambda_depth += 1

    def exit_lambda(self):
        '''Mark that a lambda/anonymous function is being exited.'''
        if self.lambda_depth > 0:
            self.lambda_depth -= 1


def get_method(cls, name):
    """ python3 doesn't need the __func__ to get the func of the
        method.
    """
    method = getattr(cls, name)
    if hasattr(method, "__func__"):
        method = method.__func__
    return method


def patch(frm, accept_class):
    for method in [k for k in frm.__dict__ if not k.startswith("_")]:
        setattr(accept_class, method, get_method(frm, method))


def patch_append_method(frm, accept_class, method_name):
    old_method = get_method(accept_class, method_name)

    def appended(*args, **kargs):
        old_method(*args, **kargs)
        frm(*args, **kargs)

    setattr(accept_class, method_name, appended)


def _init_cogc_data(self, *_):
    """Initialize Cognitive Complexity tracking fields in FunctionInfo."""
    self.cognitive_complexity = 0
    self.initial_nesting_level = 0  # Track bracket-based nesting level at function start (for CogC)
    self.initial_cogc_nesting_level = 0  # Track control-flow nesting at function start (for preprocessor directives)
    self.cognitive_complexity_increments = []  # Track line-by-line CogC increments for debugging/reporting


def _init_cogc_context_data(self, filename):
    """Initialize Cognitive Complexity context variables in FileInfoBuilder."""
    # Cognitive complexity tracking
    self.cogc_nesting_level = 0
    self.cogc_last_operator = None  # Track last binary logical operator seen (normalized)
    self.cogc_nesting_stack = []  # Track which nesting levels are from structural control flow
    self.cogc_excluded_nesting = 0  # Track nesting levels that don't count (try blocks)
    self.pending_lambda_nesting = False  # Track if next brace should increase nesting for lambda
    self.lambda_depth = 0  # Track how many lambdas deep we are (for JavaScript nested lambdas)
    self.has_top_level_increment = False  # Track if current function has top-level structural increments (JavaScript)
    self.pending_structural_nesting = False  # Track if next bare nesting is for structural keyword (brace-less languages)

    # Register hooks for nesting tracking
    if not hasattr(self, '_bare_nesting_hooks'):
        self._bare_nesting_hooks = []
    if not hasattr(self, '_pop_nesting_hooks'):
        self._pop_nesting_hooks = []
    self._bare_nesting_hooks.append(_cogc_bare_nesting_hook)
    self._pop_nesting_hooks.append(_cogc_pop_nesting_hook)


def _reset_cogc_on_push_function(self, name):
    """Reset CogC nesting when pushing a new function (for nested functions)."""
    self.reset_cogc_nesting()


def _init_cogc_on_try_new_function(self, name):
    """Store initial nesting levels for CogC calculation."""
    # Store the initial nesting level for cognitive complexity calculation
    # This accounts for classes/namespaces that the function is nested in
    self.current_function.initial_nesting_level = self.current_nesting_level
    # Store the initial cogc nesting separately (e.g., from preprocessor directives)
    # This will be added to cogc-based nesting calculations
    self.current_function.initial_cogc_nesting_level = self.cogc_nesting_level


def _inherit_cogc_from_global(self):
    """Inherit CogC from global if function is inside preprocessor block."""
    # If the function is inside a preprocessor block (cogc_nesting_level > 0),
    # inherit the CogC from global preprocessor directives
    if self.current_function.initial_cogc_nesting_level > 0:
        self.current_function.cognitive_complexity = self.global_pseudo_function.cognitive_complexity


# Lazy import to avoid circular dependency (lizard.py imports this module at load time)
import sys
# Handle both 'lizard' module import and '__main__' execution
if '__main__' in sys.modules and hasattr(sys.modules['__main__'], 'FileInfoBuilder'):
    # Running as script (python lizard.py)
    main_module = sys.modules['__main__']
    FileInfoBuilder = main_module.FileInfoBuilder
    FunctionInfo = main_module.FunctionInfo
else:
    # Normal import (from lizard import ...)
    from lizard import FileInfoBuilder, FunctionInfo

# Monkey-patch FileInfoBuilder and FunctionInfo
patch(CogCFileInfoAddition, FileInfoBuilder)
patch_append_method(_init_cogc_data, FunctionInfo, "__init__")
patch_append_method(_init_cogc_context_data, FileInfoBuilder, "__init__")
patch_append_method(_reset_cogc_on_push_function, FileInfoBuilder, "push_new_function")
patch_append_method(_init_cogc_on_try_new_function, FileInfoBuilder, "try_new_function")
patch_append_method(_inherit_cogc_from_global, FileInfoBuilder, "confirm_new_function")


def format_cogc_line_breakdown(function):
    """Format line-by-line cognitive complexity breakdown for a function.

    Args:
        function: FunctionInfo object with cognitive_complexity_increments

    Returns:
        String with formatted line-by-line breakdown
    """
    if not hasattr(function, 'cognitive_complexity_increments'):
        return ""

    if not function.cognitive_complexity_increments:
        return ""

    # Group by line to show all increments for each line together
    line_increments = {}
    for inc in function.cognitive_complexity_increments:
        line = inc['line']
        if line not in line_increments:
            line_increments[line] = []
        line_increments[line].append(inc)

    output = []
    # Add a clear separator and function identifier
    output.append("")
    output.append("  " + "=" * 78)
    output.append(f"  Cognitive Complexity Breakdown for: {function.name}")
    output.append(f"  Location: {function.filename}:{function.start_line}-{function.end_line}")
    output.append("  " + "=" * 78)
    output.append(f"  {'Line':<6} {'Inc':<5} {'Nesting':<8} {'Reason':<30} {'Token':<15}")
    output.append(f"  {'-'*78}")

    for line in sorted(line_increments.keys()):
        for inc in line_increments[line]:
            output.append(f"  {inc['line']:<6} "
                         f"+{inc['increment']:<4} "
                         f"{inc['nesting']:<8} "
                         f"{inc['reason']:<30} "
                         f"{inc['token']:<15}")

    output.append(f"  {'-'*78}")
    output.append(f"  Total Cognitive Complexity: {function.cognitive_complexity}")
    output.append("  " + "=" * 78)
    output.append("")

    return "\n".join(output)
