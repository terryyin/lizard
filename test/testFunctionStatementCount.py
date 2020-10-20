import unittest

from lizard_ext.lizardstatementcount import LizardExtension as StatementCounter
from .testHelpers import get_cpp_function_list_with_extension, get_python_function_list_with_extension


class TestFunctionStatementCount(unittest.TestCase):

    def test_unsupported_language(self):
        code = """
        def Hello_World():
            print("Hello World")

        Hello_World()
        """
        result = get_python_function_list_with_extension(code, StatementCounter())
        self.assertEqual(None, result[0].statement_count)

    def test_empty_function_should_count_as_0(self):
        code = """
        int fun()
        {
        }
        """
        result = get_cpp_function_list_with_extension(code, StatementCounter())
        self.assertEqual(0, result[0].statement_count)

    def test_function_with_return_count_as_1(self):
        code = """
        int fun()
        {
            return 0;     /* 1 */
        }
        """
        result = get_cpp_function_list_with_extension(code, StatementCounter())
        self.assertEqual(1, result[0].statement_count)

    def test_function_with_if_else_count_as_5(self):
        code = """
        int fun()
        {
            if (0)        /* 1 */
            {             /* 2 */
                return 0; /* 3 */
            }
            else
            {             /* 4 */
                return 1; /* 5 */
            }
        }
        """
        result = get_cpp_function_list_with_extension(
            code, StatementCounter())
        self.assertEqual(5, result[0].statement_count)

    def test_function_with_for_count_as_7(self):
        code = """
        int fun()
        {
            int i;              /* 1 */
            for (i=0;i<2;i++)   /* 2,3,4 */
            {                   /* 5 */
                print("%d",i);  /* 6 */
            }
            return 1;           /* 7 */
        }
        """
        result = get_cpp_function_list_with_extension(
            code,
            StatementCounter())
        self.assertEqual(7, result[0].statement_count)

    def test_function_with_while_count_as_6(self):
        code = """
        int fun()
        {
            int i=0;            /* 1 */ 
            while(i<2)          /* 2 */
            {                   /* 3 */
                print("%d",i);  /* 4 */
                i++;            /* 5 */
            }
            return 1;           /* 6 */
        }
        """
        result = get_cpp_function_list_with_extension(
            code,
            StatementCounter())
        self.assertEqual(6, result[0].statement_count)

    def test_function_with_while_count_as_10(self):
        # Using the example from: https://ator1699.home.xs4all.nl/Work/GBS/Doc_and_download/doc/metrics.html#STST1
        code = """
        void stst( void )
        {
            int i;          /* 1 */
        label_1:            /* 2 */
        label_2:            /* 3 */
            switch ( 1 )    /* 4 */
            {               /* 5 */
            case 0:         /* 6 */
            case 1:         /* 7 */
            case 2:         /* 8 */
            default:        /* 9 */
                break;      /* 10 */
            }
        }"""
        result = get_cpp_function_list_with_extension(code, StatementCounter())
        self.assertEqual(10, result[0].statement_count)
