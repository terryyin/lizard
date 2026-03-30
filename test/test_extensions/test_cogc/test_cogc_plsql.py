"""Cognitive Complexity tests for PLSQL"""
import unittest
from lizard import FileAnalyzer, get_extensions
from lizard_ext.lizardcogc import LizardExtension as CogC


def get_plsql_cogc(source_code):
    """Analyze PLSQL code with Cognitive Complexity extension"""
    return FileAnalyzer(get_extensions([CogC()])).analyze_source_code(
        "test.sql", source_code
    ).function_list


class TestPlsqlCognitiveComplexity(unittest.TestCase):
    """Cognitive Complexity tests for PL/SQL"""

    def test_simple_function_has_zero_cogc(self):
        """Simple straight-line function should have CogC=0"""
        code = """
CREATE OR REPLACE FUNCTION simple_func RETURN NUMBER IS
    v_result NUMBER;
BEGIN
    v_result := 5 * 2;
    RETURN v_result;
END;
"""
        functions = get_plsql_cogc(code)
        self.assertEqual(0, functions[0].cognitive_complexity)

    def test_single_if_statement(self):
        """Single IF statement should be CogC=1"""
        code = """
CREATE OR REPLACE FUNCTION check_value(p_value NUMBER) RETURN VARCHAR2 IS
BEGIN
    IF p_value > 0 THEN         -- +1
        RETURN 'positive';
    END IF;
    RETURN 'non-positive';
END;
"""
        functions = get_plsql_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_nested_loops_with_nesting_penalty(self):
        """Nested structures demonstrate nesting penalty multiplication"""
        code = """
CREATE OR REPLACE FUNCTION nested_loops RETURN NUMBER IS
    v_count NUMBER := 0;
BEGIN
    FOR i IN 1..10 LOOP                 -- +1
        FOR j IN 1..10 LOOP             -- +2 (nesting=1)
            IF i = j THEN                -- +3 (nesting=2)
                v_count := v_count + 1;
            END IF;
        END LOOP;
    END LOOP;
    RETURN v_count;
END;  -- Total CogC = 6
"""
        functions = get_plsql_cogc(code)
        self.assertEqual(6, functions[0].cognitive_complexity)

    def test_case_statement_counts_as_one(self):
        """CASE statement counts as 1 regardless of number of branches"""
        code = """
CREATE OR REPLACE FUNCTION get_day(p_day NUMBER) RETURN VARCHAR2 IS
BEGIN
    RETURN CASE p_day           -- +1
        WHEN 1 THEN 'Monday'
        WHEN 2 THEN 'Tuesday'
        WHEN 3 THEN 'Wednesday'
        ELSE 'Unknown'
    END;
END;  -- Total CogC = 1
"""
        functions = get_plsql_cogc(code)
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_binary_logical_operators(self):
        """Binary logical operator sequences"""
        code = """
CREATE OR REPLACE FUNCTION complex_condition(a BOOLEAN, b BOOLEAN, c BOOLEAN,
                                             d BOOLEAN, e BOOLEAN) RETURN BOOLEAN IS
BEGIN
    IF a AND b AND c THEN       -- +1 for IF, +1 for AND sequence
        RETURN TRUE;
    END IF;
    IF d OR e THEN              -- +1 for IF, +1 for OR sequence
        RETURN FALSE;
    END IF;
    RETURN FALSE;
END;  -- Total CogC = 4
"""
        functions = get_plsql_cogc(code)
        self.assertEqual(4, functions[0].cognitive_complexity)

    def test_exception_handler(self):
        """Exception handler (EXCEPTION WHEN) should add complexity"""
        code = """
CREATE OR REPLACE FUNCTION safe_divide(p_num NUMBER, p_den NUMBER) RETURN NUMBER IS
BEGIN
    RETURN p_num / p_den;
EXCEPTION
    WHEN ZERO_DIVIDE THEN       -- +1
        RETURN NULL;
    WHEN OTHERS THEN            -- +1
        RAISE;
END;  -- Total CogC = 2
"""
        functions = get_plsql_cogc(code)
        self.assertEqual(2, functions[0].cognitive_complexity)

    def test_cursor_loop(self):
        """Cursor FOR loop counts as loop"""
        code = """
CREATE OR REPLACE FUNCTION process_employees RETURN NUMBER IS
    v_count NUMBER := 0;
BEGIN
    FOR emp_rec IN (SELECT * FROM employees) LOOP   -- +1
        IF emp_rec.salary > 50000 THEN              -- +2 (nesting=1)
            v_count := v_count + 1;
        END IF;
    END LOOP;
    RETURN v_count;
END;  -- Total CogC = 3
"""
        functions = get_plsql_cogc(code)
        self.assertEqual(3, functions[0].cognitive_complexity)

    def test_string_concatenation_operator_not_counted(self):
        """|| operator in PL/SQL is string concatenation, NOT logical OR"""
        code = """
CREATE OR REPLACE FUNCTION build_message(p_name VARCHAR2, p_status VARCHAR2) RETURN VARCHAR2 IS
    v_message VARCHAR2(200);
BEGIN
    -- || is concatenation in SQL, should NOT add to CogC
    v_message := 'User: ' || p_name || ' Status: ' || p_status;

    -- AND is still a logical operator and should count
    IF p_name IS NOT NULL AND p_status = 'ACTIVE' THEN   -- +1 IF, +1 AND = +2
        v_message := v_message || ' (verified)';
    END IF;

    RETURN v_message;
END;  -- Total CogC = 2 (IF and AND only, NOT the || concatenations)
"""
        functions = get_plsql_cogc(code)
        # Should be 2: +1 for IF, +1 for AND
        # The four || operators should NOT be counted (they're string concatenation)
        self.assertEqual(2, functions[0].cognitive_complexity)

    def test_string_concatenation_with_rownum(self):
        """Real-world pattern: || concatenation with AND in WHERE clause"""
        code = """
CREATE OR REPLACE FUNCTION get_chart_name(p_id NUMBER) RETURN VARCHAR2 IS
    v_name VARCHAR2(100);
BEGIN
    SELECT 'Chart_' || chart_type || '_' || chart_id
    INTO v_name
    FROM charts
    WHERE chart_id = p_id
    AND ROWNUM = 1;                     -- +1 for AND

    RETURN v_name;
EXCEPTION
    WHEN NO_DATA_FOUND THEN             -- +1 for exception handler
        RETURN NULL;
END;  -- Total CogC = 2 (AND and WHEN only, NOT the || concatenations)
"""
        functions = get_plsql_cogc(code)
        # Should be 2: +1 for AND in WHERE, +1 for WHEN
        # The || operators in SELECT should NOT be counted
        self.assertEqual(2, functions[0].cognitive_complexity)
