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
