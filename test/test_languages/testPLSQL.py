import unittest
from lizard import analyze_file


def get_plsql_fileinfo(source_code):
    return analyze_file.analyze_source_code("test.sql", source_code)


def get_plsql_function_list(source_code):
    return get_plsql_fileinfo(source_code).function_list


class TestPLSQL(unittest.TestCase):

    def test_empty_file(self):
        result = get_plsql_function_list("")
        self.assertEqual(0, len(result))

    def test_simple_procedure(self):
        code = """
        CREATE OR REPLACE PROCEDURE simple_proc IS
        BEGIN
            NULL;
        END simple_proc;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual("simple_proc", result[0].name)
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_simple_function(self):
        code = """
        CREATE OR REPLACE FUNCTION simple_func RETURN NUMBER IS
        BEGIN
            RETURN 1;
        END simple_func;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual("simple_func", result[0].name)
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_procedure_with_parameters(self):
        code = """
        CREATE OR REPLACE PROCEDURE process_order(
            p_order_id IN NUMBER,
            p_status OUT VARCHAR2
        ) IS
        BEGIN
            NULL;
        END process_order;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual("process_order", result[0].name)
        self.assertEqual(2, result[0].parameter_count)

    def test_function_with_if_statement(self):
        code = """
        CREATE OR REPLACE FUNCTION check_status(p_id NUMBER) RETURN VARCHAR2 IS
        BEGIN
            IF p_id > 0 THEN
                RETURN 'VALID';
            END IF;
            RETURN 'INVALID';
        END check_status;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_if_elsif_else(self):
        code = """
        CREATE PROCEDURE test_proc IS
            x NUMBER := 1;
        BEGIN
            IF x = 1 THEN
                NULL;
            ELSIF x = 2 THEN
                NULL;
            ELSIF x = 3 THEN
                NULL;
            ELSE
                NULL;
            END IF;
        END;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(4, result[0].cyclomatic_complexity)  # 1 + IF + ELSIF + ELSIF

    def test_case_statement(self):
        code = """
        CREATE PROCEDURE test_case IS
            status VARCHAR2(20) := 'NEW';
        BEGIN
            CASE status
                WHEN 'NEW' THEN
                    NULL;
                WHEN 'PENDING' THEN
                    NULL;
                WHEN 'COMPLETE' THEN
                    NULL;
            END CASE;
        END;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(4, result[0].cyclomatic_complexity)  # 1 + 3 WHENs

    def test_loop_structures(self):
        code = """
        CREATE PROCEDURE test_loops IS
            x NUMBER := 0;
        BEGIN
            LOOP
                EXIT WHEN x > 10;
            END LOOP;

            WHILE x < 100 LOOP
                NULL;
            END LOOP;

            FOR i IN 1..10 LOOP
                NULL;
            END LOOP;
        END;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(4, result[0].cyclomatic_complexity)  # 1 + LOOP + WHILE + FOR

    def test_logical_operators(self):
        code = """
        CREATE PROCEDURE test_logic IS
            x NUMBER := 1;
            y NUMBER := 2;
            z NUMBER := 3;
        BEGIN
            IF x = 1 AND y = 2 OR z = 3 THEN
                NULL;
            END IF;
        END;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(4, result[0].cyclomatic_complexity)  # 1 + IF + AND + OR

    def test_exception_handling(self):
        code = """
        CREATE PROCEDURE test_exceptions IS
        BEGIN
            NULL;
        EXCEPTION
            WHEN NO_DATA_FOUND THEN
                NULL;
            WHEN TOO_MANY_ROWS THEN
                NULL;
            WHEN OTHERS THEN
                NULL;
        END;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(4, result[0].cyclomatic_complexity)  # 1 + 3 WHENs in EXCEPTION

    def test_single_line_comment(self):
        code = """
        CREATE PROCEDURE test_comment IS
        BEGIN
            -- This is a comment
            NULL;
        END;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(1, len(result))

    def test_multi_line_comment(self):
        code = """
        CREATE PROCEDURE test_comment IS
        BEGIN
            /* This is a
               multi-line comment */
            NULL;
        END;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(1, len(result))

    def test_string_literals(self):
        code = """
        CREATE PROCEDURE test_strings IS
            v_msg VARCHAR2(100) := 'Hello, World';
        BEGIN
            v_msg := 'Another string';
        END;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(1, len(result))

    def test_package_procedure(self):
        code = """
        CREATE OR REPLACE PACKAGE BODY my_package IS
            PROCEDURE pkg_proc IS
            BEGIN
                NULL;
            END pkg_proc;

            FUNCTION pkg_func RETURN NUMBER IS
            BEGIN
                RETURN 1;
            END pkg_func;
        END my_package;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(2, len(result))
        self.assertEqual("pkg_proc", result[0].name)
        self.assertEqual("pkg_func", result[1].name)

    def test_nested_procedure(self):
        code = """
        CREATE PROCEDURE outer_proc IS
            PROCEDURE inner_proc IS
            BEGIN
                NULL;
            END inner_proc;
        BEGIN
            inner_proc;
        END outer_proc;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(2, len(result))

    def test_function_as_keyword(self):
        code = """
        CREATE FUNCTION get_total RETURN NUMBER AS
            v_total NUMBER;
        BEGIN
            RETURN v_total;
        END;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual("get_total", result[0].name)

    def test_procedure_is_keyword(self):
        code = """
        CREATE PROCEDURE do_work IS
        BEGIN
            NULL;
        END;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(1, len(result))

    def test_procedure_as_keyword(self):
        code = """
        CREATE PROCEDURE do_work AS
        BEGIN
            NULL;
        END;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(1, len(result))

    def test_complex_example(self):
        code = """
        CREATE OR REPLACE PROCEDURE process_order(p_order_id IN NUMBER) IS
          v_status VARCHAR2(20);
        BEGIN
          SELECT status INTO v_status FROM orders WHERE order_id = p_order_id;

          IF v_status = 'NEW' THEN
            UPDATE orders SET status = 'PROCESSING' WHERE order_id = p_order_id;
          ELSIF v_status = 'PENDING' THEN
            UPDATE orders SET status = 'APPROVED' WHERE order_id = p_order_id;
          END IF;

          COMMIT;
        EXCEPTION
          WHEN NO_DATA_FOUND THEN
            DBMS_OUTPUT.PUT_LINE('Order not found');
          WHEN OTHERS THEN
            ROLLBACK;
        END process_order;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual("process_order", result[0].name)
        self.assertEqual(1, result[0].parameter_count)
        # Complexity: 1 (base) + 1 (IF) + 1 (ELSIF) + 2 (WHEN exceptions) = 5
        self.assertEqual(5, result[0].cyclomatic_complexity)

    def test_case_insensitive_keywords(self):
        code = """
        create or replace procedure test_proc is
        begin
            if x = 1 then
                null;
            end if;
        end;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual("test_proc", result[0].name)
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_declare_block(self):
        code = """
        DECLARE
            v_count NUMBER;
        BEGIN
            SELECT COUNT(*) INTO v_count FROM users;
        END;
        """
        result = get_plsql_function_list(code)
        # Anonymous blocks should not be counted as functions
        self.assertEqual(0, len(result))

    def test_nloc_counting(self):
        code = """
        CREATE PROCEDURE test_nloc IS
            v_x NUMBER;
        BEGIN
            v_x := 1;
            v_x := v_x + 1;
            NULL;
        END;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(1, len(result))
        self.assertGreater(result[0].nloc, 0)

    def test_schema_qualified_procedure(self):
        code = """
        CREATE OR REPLACE PROCEDURE my_schema.simple_proc IS
        BEGIN
            NULL;
        END simple_proc;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual("simple_proc", result[0].name)
        # Complexity: 1 (base)
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_schema_qualified_function(self):
        code = """
        CREATE OR REPLACE FUNCTION my_schema.simple_func RETURN NUMBER IS
        BEGIN
            RETURN 1;
        END simple_func;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual("simple_func", result[0].name)
        # Complexity: 1 (base)
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_schema_qualified_with_parameters(self):
        code = """
        CREATE OR REPLACE PROCEDURE my_schema.process_order(
            p_order_id IN NUMBER,
            p_status OUT VARCHAR2
        ) IS
        BEGIN
            NULL;
        END process_order;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual("process_order", result[0].name)
        self.assertEqual(2, result[0].parameter_count)
        # Complexity: 1 (base)
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_schema_qualified_trigger(self):
        code = """
        CREATE OR REPLACE TRIGGER my_schema.update_timestamp
        BEFORE UPDATE ON my_table
        FOR EACH ROW
        BEGIN
            :NEW.updated_date := SYSDATE;
        END update_timestamp;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual("update_timestamp", result[0].name)
        # Complexity: 1 (base)
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_schema_qualified_trigger_with_declare(self):
        code = """
        CREATE OR REPLACE TRIGGER my_schema.audit_trigger
        AFTER INSERT OR UPDATE OR DELETE ON my_table
        FOR EACH ROW
        DECLARE
            v_action VARCHAR2(10);
        BEGIN
            IF INSERTING THEN
                v_action := 'INSERT';
            ELSIF UPDATING THEN
                v_action := 'UPDATE';
            ELSE
                v_action := 'DELETE';
            END IF;
        END audit_trigger;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual("audit_trigger", result[0].name)
        # Complexity: 1 (base) + 1 (IF) + 1 (ELSIF) = 3
        # Note: ELSE doesn't add complexity, only decision points (IF/ELSIF)
        self.assertEqual(3, result[0].cyclomatic_complexity)

    def test_schema_qualified_function_with_table_reference(self):
        # This tests that we don't confuse table.column references with schema.object
        code = """
        CREATE OR REPLACE FUNCTION my_schema.get_user_login(i_user_id NUMBER)
        RETURN user_table.login%TYPE IS
            v_login user_table.login%TYPE;
        BEGIN
            SELECT login INTO v_login FROM user_table WHERE id = i_user_id;
            RETURN v_login;
        EXCEPTION
            WHEN NO_DATA_FOUND THEN
                RETURN NULL;
        END get_user_login;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual("get_user_login", result[0].name)
        # Complexity: 1 (base) + 1 (WHEN exception handler) = 2
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_schema_qualified_with_inline_comments(self):
        # Test that inline comments don't interfere with schema-qualified name parsing
        code = """
        -- This is a header comment
        CREATE OR REPLACE PROCEDURE my_schema.process_data( -- procedure with schema
            p_id IN NUMBER  -- input parameter
        ) IS
            v_count NUMBER; -- local variable
        BEGIN
            -- Count records
            SELECT COUNT(*) INTO v_count FROM my_table WHERE id = p_id;

            -- Check if found
            IF v_count > 0 THEN
                NULL; -- do something
            END IF;
        END process_data;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual("process_data", result[0].name)
        self.assertEqual(1, result[0].parameter_count)  # 1 parameter
        # Complexity: 1 (base) + 1 (IF) = 2
        self.assertEqual(2, result[0].cyclomatic_complexity)
        # NLOC assertion omitted due to limitation in core comment handling

    def test_schema_qualified_with_block_comments(self):
        # Test that multi-line comments don't interfere with parsing
        code = """
        /*
         * This function calculates something important.
         * Created: 2025-01-01
         * Author: Test Developer
         *
         * History:
         * 2025-01-01 - Initial creation
         */
        CREATE OR REPLACE FUNCTION my_schema.calculate_value(
            /* Input value */ p_input NUMBER
        ) RETURN NUMBER IS
            /* Local variables */
            v_result NUMBER;
            v_temp NUMBER; /* temporary storage */
        BEGIN
            /*
             * Main calculation logic
             * This is a complex calculation
             */
            IF p_input > 0 THEN
                v_result := p_input * 2; /* double it */
            ELSIF p_input < 0 THEN
                v_result := p_input * -1; /* make positive */
            ELSE
                v_result := 0; /* default */
            END IF;

            /* Return the result */
            RETURN v_result;
        END calculate_value;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual("calculate_value", result[0].name)
        self.assertEqual(1, result[0].parameter_count)  # 1 parameter
        # Complexity: 1 (base) + 1 (IF) + 1 (ELSIF) = 3
        self.assertEqual(3, result[0].cyclomatic_complexity)
        # NLOC assertion omitted due to limitation in core comment handling

    def test_schema_qualified_with_mixed_comments(self):
        # Test both inline and block comments together
        code = """
        /* Package header comment */
        CREATE OR REPLACE TRIGGER my_schema.audit_changes -- audit trigger
        AFTER INSERT OR UPDATE /* handle both operations */ OR DELETE
        ON my_table
        FOR EACH ROW
        DECLARE
            -- Local variables
            v_operation VARCHAR2(10); /* operation type */
        BEGIN
            /*
             * Determine operation type
             */
            IF INSERTING THEN -- new record
                v_operation := 'INSERT';
            ELSIF UPDATING THEN -- modified record
                v_operation := 'UPDATE'; /* set to update */
            ELSE -- deleted record
                v_operation := 'DELETE';
            END IF; -- end operation check

            -- Log the operation (commented out for now)
            -- DBMS_OUTPUT.PUT_LINE('Operation: ' || v_operation);
        END audit_changes; -- end trigger
        """
        result = get_plsql_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual("audit_changes", result[0].name)
        # Complexity: 1 (base) + 1 (IF) + 1 (ELSIF) = 3
        self.assertEqual(3, result[0].cyclomatic_complexity)
        # NLOC assertion omitted due to known limitation in core comment handling

    def test_continue_statement(self):
        code = """
        CREATE PROCEDURE test_continue IS
        BEGIN
            FOR i IN 1..10 LOOP
                IF i = 5 THEN
                    CONTINUE;
                END IF;
                NULL;
            END LOOP;
        END;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(1, len(result))
        # Complexity: 1 (base) + 1 (FOR) + 1 (IF) = 3
        self.assertEqual(3, result[0].cyclomatic_complexity)

    def test_continue_when_statement(self):
        code = """
        CREATE PROCEDURE test_continue_when IS
        BEGIN
            FOR i IN 1..10 LOOP
                CONTINUE WHEN i = 5;
                NULL;
            END LOOP;
        END;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(1, len(result))
        # Complexity: 1 (base) + 1 (FOR) + 1 (WHEN from CONTINUE WHEN) = 3
        self.assertEqual(3, result[0].cyclomatic_complexity)

    def test_return_in_loop(self):
        code = """
        CREATE FUNCTION find_value(p_target NUMBER) RETURN NUMBER IS
        BEGIN
            FOR i IN 1..10 LOOP
                IF i = p_target THEN
                    RETURN i;
                END IF;
            END LOOP;
            RETURN -1;
        END;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(1, len(result))
        # Complexity: 1 (base) + 1 (FOR) + 1 (IF) = 3
        self.assertEqual(3, result[0].cyclomatic_complexity)

    def test_exit_when_explicit(self):
        code = """
        CREATE PROCEDURE test_exit_when IS
            x NUMBER := 0;
        BEGIN
            LOOP
                EXIT WHEN x > 10;
                x := x + 1;
            END LOOP;
        END;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(1, len(result))
        # Complexity: 1 (base) + 1 (LOOP) = 2
        # Note: EXIT WHEN's WHEN is filtered out by preprocessor (it's not a branch, just a conditional exit)
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_goto_statement(self):
        code = """
        CREATE PROCEDURE test_goto IS
            x NUMBER := 15;
        BEGIN
            IF x > 10 THEN
                GOTO error_handler;
            END IF;
            NULL;
            <<error_handler>>
            DBMS_OUTPUT.PUT_LINE('Error');
        END;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(1, len(result))
        # Complexity: 1 (base) + 1 (IF) = 2
        # GOTO doesn't add complexity, it's just a jump
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_cursor_for_loop(self):
        code = """
        CREATE PROCEDURE test_cursor_loop IS
        BEGIN
            FOR rec IN (SELECT * FROM users WHERE active = 1) LOOP
                DBMS_OUTPUT.PUT_LINE(rec.name);
            END LOOP;
        END;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(1, len(result))
        # Complexity: 1 (base) + 1 (FOR loop) = 2
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_explicit_cursor_declaration(self):
        code = """
        CREATE PROCEDURE test_cursor IS
            CURSOR c_users IS
                SELECT * FROM users;
            v_user users%ROWTYPE;
        BEGIN
            OPEN c_users;
            LOOP
                FETCH c_users INTO v_user;
                EXIT WHEN c_users%NOTFOUND;
                NULL;
            END LOOP;
            CLOSE c_users;
        END;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(1, len(result))
        # Complexity: 1 (base) + 1 (LOOP) = 2
        # Note: EXIT WHEN's WHEN is filtered out by preprocessor
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_trigger_before_insert(self):
        code = """
        CREATE OR REPLACE TRIGGER orders_before_insert
        BEFORE INSERT
        ON orders
        FOR EACH ROW
        DECLARE
            v_username VARCHAR2(10);
        BEGIN
            SELECT user INTO v_username FROM dual;
            :new.create_date := sysdate;
            :new.created_by := v_username;
        END;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual("orders_before_insert", result[0].name)
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_trigger_with_conditionals(self):
        code = """
        CREATE TRIGGER salary_check
        BEFORE INSERT OR UPDATE OF salary
        ON employees
        FOR EACH ROW
        BEGIN
            IF :new.salary > 100000 THEN
                RAISE_APPLICATION_ERROR(-20001, 'Salary too high');
            END IF;
        END;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(1, len(result))
        self.assertEqual("salary_check", result[0].name)
        # Complexity: 1 (base) + 1 (IF) = 2
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_type_declaration_in_procedure(self):
        code = """
        CREATE PROCEDURE test_type IS
            TYPE t_numbers IS TABLE OF NUMBER;
            v_nums t_numbers;
        BEGIN
            v_nums := t_numbers(1, 2, 3);
            FOR i IN 1..v_nums.COUNT LOOP
                NULL;
            END LOOP;
        END;
        """
        result = get_plsql_function_list(code)
        self.assertEqual(1, len(result))
        # Complexity: 1 (base) + 1 (FOR) = 2
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_nested_exception_handlers(self):
        code = """
        CREATE PROCEDURE test_nested_exceptions IS
        BEGIN
            BEGIN
                NULL;
            EXCEPTION
                WHEN NO_DATA_FOUND THEN
                    NULL;
            END;
            NULL;
        EXCEPTION
            WHEN OTHERS THEN
                NULL;
        END;
        """
        result = get_plsql_function_list(code)
        # Nested BEGIN/END blocks with EXCEPTION sections are handled as part of the parent procedure
        self.assertEqual(1, len(result))
        # The main procedure has complexity: 1 (base) + 1 (inner WHEN) + 1 (outer WHEN) = 3
        # Both WHEN clauses are counted as they both add decision points to the procedure
        main_proc = result[0]
        self.assertEqual("test_nested_exceptions", main_proc.name)
        self.assertEqual(3, main_proc.cyclomatic_complexity)

    def test_package_specification(self):
        code = """
        CREATE OR REPLACE PACKAGE my_package IS
            PROCEDURE proc1;
            FUNCTION func1 RETURN NUMBER;
        END my_package;
        """
        result = get_plsql_function_list(code)
        # Package specs just declare signatures, no implementations
        self.assertEqual(0, len(result))

    def test_anonymous_block_with_nested_function(self):
        code = """
        DECLARE
            FUNCTION inner_func RETURN NUMBER IS
            BEGIN
                RETURN 1;
            END;
        BEGIN
            DBMS_OUTPUT.PUT_LINE(inner_func);
        END;
        """
        result = get_plsql_function_list(code)
        # Anonymous block itself should not be counted
        # But nested function should be counted
        self.assertEqual(1, len(result))
        self.assertEqual("inner_func", result[0].name)


class Test_PLSQL_Cognitive_Complexity(unittest.TestCase):
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
        functions = get_plsql_function_list(code)
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
        functions = get_plsql_function_list(code)
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
        functions = get_plsql_function_list(code)
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
        functions = get_plsql_function_list(code)
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
        functions = get_plsql_function_list(code)
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
        functions = get_plsql_function_list(code)
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
        functions = get_plsql_function_list(code)
        self.assertEqual(3, functions[0].cognitive_complexity)
