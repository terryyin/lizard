import unittest
from ..testHelpers import get_st_fileinfo, get_st_function_list, get_st_function_list_with_extension
from lizard_ext.lizardnd import LizardExtension as NestDepth


def get_st_with_nestdepth(source):
    return get_st_function_list_with_extension(source, NestDepth())


class Test_st_preprocessing(unittest.TestCase):

    def test_preprocessor_is_not_function(self):
        result = get_st_function_list('''
                #ifdef A
                #elif (defined E)
                #endif
                ''')
        self.assertEqual(0, len(result))


class Test_st_lizard(unittest.TestCase):

    def test_empty(self):
        result = get_st_function_list("")
        self.assertEqual(0, len(result))

    def test_comment(self):
        result = get_st_fileinfo('(* Comment1 *)\nint fun(){}\n(* Comment2 *)')
        self.assertEqual(1, result.nloc)

    def test_no_function(self):
        result = get_st_function_list('(* Comment1 *)\n')
        self.assertEqual(0, len(result))

    def test_one_function(self):
        result = get_st_function_list(
            "(* Comment *)\n"
            "FUNCTION fun:\n"
            "// Comment\n"
            "foo := bar;\n"
            "END_FUNCTION\n"
            )
        self.assertEqual(1, len(result))
        self.assertEqual("fun", result[0].name)

    def test_two_function(self):
        result = get_st_function_list(   
            "FUNCTION fun1:\n"
            "// Comment\n"
            "foo := bar;\n"
            "END_FUNCTION\n"
            "(* Comment *)\n"
            "FUNCTION fun2:\n"
            "// Comment\n"
            "foo := bar;\n"
            "END_FUNCTION\n")
        self.assertEqual(2, len(result))
        self.assertEqual("fun1", result[0].name)
        self.assertEqual("fun2", result[1].name)
        self.assertEqual(1, result[0].start_line)
        self.assertEqual(4, result[0].end_line)
        self.assertEqual(6, result[1].start_line)
        self.assertEqual(9, result[1].end_line)

    def test_one_action(self):
        result = get_st_function_list(
            "(* Comment *)\n"
            "ACTION ac1:\n"
            "// Comment\n"
            "foo := bar;\n"
            "END_ACTION\n"
            )
        self.assertEqual(1, len(result))
        self.assertEqual("ac1", result[0].name)

    def test_two_actions(self):
        ''' Action is handled as function without parameter. '''
        result = get_st_function_list(
            "ACTION ac1:\n"
            "// Comment\n"
            "foo := bar;\n"
            "END_ACTION\n"
            "(* Comment *)\n"
            "ACTION ac2:\n"
            "// Comment\n"
            "foo := bar;\n"
            "END_ACTION\n")
        self.assertEqual(2, len(result))
        self.assertEqual("ac1", result[0].name)
        self.assertEqual("ac2", result[1].name)
        self.assertEqual(1, result[0].start_line)
        self.assertEqual(4, result[0].end_line)
        self.assertEqual(6, result[1].start_line)
        self.assertEqual(9, result[1].end_line)

    def test_nested_actions(self):
        ''' Nesting Depth of Functions is 1. '''
        result = get_st_function_list(
            '''
            ACTION ac1:
                // Comment
                action := bar;
                bar := ACTION;
                function := bar;
                bar := FUNCTION;
            END_ACTION
            ''')
        self.assertEqual(1, len(result))
        self.assertEqual("ac1", result[0].name)
        self.assertEqual(2, result[0].start_line)
        self.assertEqual(8, result[0].end_line)


class Test_st_cyclomatic_complexity(unittest.TestCase):

    def test_cc_if(self):
        result = get_st_function_list(
            """
            ACTION ac1:
                a := 200;
                IF b THEN
                    c := 1;
                END_IF
            END_ACTION
            """)
        self.assertEqual(1, len(result))
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_cc_if_else(self):
        result = get_st_function_list(
            """
            ACTION ac1:
                a := 200;
                IF b THEN
                    c := 1;
                ELSE:
                    c := 2;
                END_IF
            END_ACTION
            """)
        self.assertEqual(1, len(result))
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_cc_case(self):
        result = get_st_function_list(
            """
            ACTION ac1:
                a := 200;
                CASE a OF
                    b1:
                        c := 1;
                    b2:
                        c := 2;
                END_CASE
            END_ACTION
            """)
        self.assertEqual(1, len(result))
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_cc_case_else(self):
        result = get_st_function_list(
            """
            ACTION ac1:
                a := 200;
                CASE a OF
                    b1:
                        c := 1;
                    b2:
                        c := 2;
                    else:
                        c := 3;
                END_CASE
            END_ACTION
            """)
        self.assertEqual(1, len(result))
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_cc_for(self):
        result = get_st_function_list(
            """
            ACTION ac1:
                a := 10;
                FOR b := 1 TO a DO
                    c := c + b;
                END_FOR
            END_ACTION
            """)
        self.assertEqual(1, len(result))
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_cc_while(self):
        result = get_st_function_list(
            """
            ACTION ac1:
                a := 10;
                WHILE b < a DO
                    c := c + b;
                    b := b + 1;
                END_WHILE
            END_ACTION
            """)
        self.assertEqual(1, len(result))
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_cc_repeat(self):
        result = get_st_function_list(
            """
            ACTION ac1:
                REPEAT
                    c := c + b;
                    b := b + 1;
                    UNTIL b < a
                END_REPEAT
            END_ACTION
            """)
        self.assertEqual(1, len(result))
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_cc_nested(self):
        result = get_st_with_nestdepth(
            """
            (* Flat *)
            ACTION acFlat:
                a := 1;
            END_ACTION

            (* Nested *)
            ACTION acNested:
                IF a THEN
                    CASE a OF
                        0:
                            FOR b := 1 TO a DO
                                WHILE b < a DO
                                    REPEAT
                                        c := 1
                                        UNTIL b < a
                                    END_REPEAT
                                END_WHILE
                            END_FOR
                        ELSE:
                            c := -2;
                    END_CASE
                ELSE:
                    c := -1;
                END_IF
            END_ACTION
            """)
        self.assertEqual(2, len(result))
        self.assertEqual(1, result[0].cyclomatic_complexity)
        self.assertEqual(6, result[1].cyclomatic_complexity)

    def test_logical_operators_and_or(self):
        """
        BUG: ST (Structured Text) has AND/OR logical operators but they were
        NOT in the original _conditions set. This test checks if they should be.
        
        ST uses AND, OR for boolean logic (case-insensitive).
        """
        result = get_st_function_list(
            """
            ACTION test_logic:
                IF (a > 0) AND (b > 0) OR (c > 0) THEN
                    x := 1;
                END_IF
                
                IF (d > 0) AND (e > 0) THEN
                    y := 2;
                END_IF
            END_ACTION
            """)
        self.assertEqual(1, len(result))
        
        # Fixed: AND/OR operators now counted
        # Expected: base(1) + IF(1) + AND(1) + OR(1) + IF(1) + AND(1) = 6
        current_ccn = result[0].cyclomatic_complexity
        
        # Bug fixed: AND/OR operators should be counted like other languages
        self.assertEqual(6, current_ccn,
                        "ST should count AND/OR logical operators")


class Test_st_nesting_level(unittest.TestCase):

    def test_st_nd_level_flat(self):
        result = get_st_with_nestdepth(
            """
            (* Flat1 *)
            ACTION acFlat1:
                a := 1;
            END_ACTION

            (* Flat2 *)
            ACTION acFlat2:
                IF a THEN
                    c := 1;
                END_IF

                IF b THEN
                    c := 2;
                END_IF
            END_ACTION
            """)
        self.assertEqual(0, result[1].top_nesting_level)
        self.assertEqual(1, result[1].max_nesting_depth)

    def test_st_nd_level_nested(self):
        result = get_st_with_nestdepth(
            """
            (* Flat *)
            ACTION acFlat:
                a := 1;
            END_ACTION

            (* Nested *)
            ACTION acNested:
                IF a THEN
                    CASE a OF
                        0:
                            FOR b := 1 TO a DO
                                WHILE b < a DO
                                    REPEAT
                                        c := 1
                                        UNTIL b < a
                                    END_REPEAT
                                END_WHILE
                            END_FOR
                        ELSE:
                            c := -2;
                    END_CASE
                ELSE:
                    c := -1;
                END_IF
            END_ACTION
            """)
        self.assertEqual(0, result[1].top_nesting_level)
        self.assertEqual(5, result[1].max_nesting_depth)

    def test_st_nd_level_max(self):
        result = get_st_with_nestdepth(
            """
            (* Nested 1 *)
            ACTION acNested1:
                IF a THEN
                    IF b THEN
                        c := 1;
                    END_IF
                END_IF
            END_ACTION

            (* Nested 2 *)
            ACTION acNested2:
                IF a THEN
                    IF b THEN
                        IF c THEN
                            d := 1;
                        END_IF
                    END_IF

                    if NOT b THEN
                        b := 1;
                    END_IF
                END_I
            END_ACTION
            """)
        self.assertEqual(0, result[1].top_nesting_level)
        self.assertEqual(3, result[1].max_nesting_depth)

    def test_st_nd_level_case(self):
        result = get_st_with_nestdepth(
            '''
            ACTION ac1 :
                FOR i := 0 TO 1 DO
                    CASE a OF

                        1:

                            IF b THEN
                                c := 1;
                                IF b THEN
                                    IF b THEN
                                        c := 2;
                                    ELSIF b THEN
                                        c := 2;
                                    ELSIF b THEN
                                        c := 2;
                                    END_IF
                                END_IF;
                            END_IF

                        2:
                            IF b THEN
                                IF b THEN
                                    c := 3;
                                END_IF
                            END_IF

                            IF b THEN
                                c := 3;
                            END_IF

                    END_CASE

                END_FOR;

            END_ACTION
            '''
        )
        self.assertEqual(0, result[0].top_nesting_level)
        self.assertEqual(4, result[0].max_nesting_depth)
