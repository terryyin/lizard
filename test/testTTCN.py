import unittest
import inspect
from lizard import analyze_file, FileAnalyzer, get_extensions


def get_ttcn_function_list(source_code):
    return analyze_file.analyze_source_code(
        "a.ttcn", source_code).function_list


class Test_parser_for_TTCN(unittest.TestCase):

    def test_empty(self):
        functions = get_ttcn_function_list("")
        self.assertEqual(0, len(functions))

    def test_no_function(self):
        result = get_ttcn_function_list('''
                module test {
                    const integer i := 1;
                }''')
        self.assertEqual(0, len(result))

    def test_one_function(self):
        result = get_ttcn_function_list('''
                module test_one_func {
                    function fun() {}
                }''')
        self.assertEqual(1, len(result))
        self.assertEqual("fun", result[0].name)

    def test_two_function(self):
        result = get_ttcn_function_list('''
                module test_two_func {
                    function fun()
                    {
                    }
                    function fun1()
                    {
                    }
                }
                ''')
        self.assertEqual(2, len(result))
        self.assertEqual("fun", result[0].name)
        self.assertEqual("fun1", result[1].name)
        self.assertEqual(3, result[0].start_line)
        self.assertEqual(5, result[0].end_line)
        self.assertEqual(6, result[1].start_line)
        self.assertEqual(8, result[1].end_line)

    def test_one_testcase(self):
        result = get_ttcn_function_list('''
                module test_one_tc {
                    testcase tc() {}
                }''')
        self.assertEqual(1, len(result))
        self.assertEqual("__testcase__tc", result[0].name)

    def test_two_testcase(self):
        result = get_ttcn_function_list('''
                module test_two_tc {
                    testcase tc()
                    {
                    }
                    testcase tc1()
                    {
                    }
                }
                ''')
        self.assertEqual(2, len(result))
        self.assertEqual("__testcase__tc", result[0].name)
        self.assertEqual("__testcase__tc1", result[1].name)
        self.assertEqual(3, result[0].start_line)
        self.assertEqual(5, result[0].end_line)
        self.assertEqual(6, result[1].start_line)
        self.assertEqual(8, result[1].end_line)

    def test_one_function_one_testcase(self):
        result = get_ttcn_function_list('''
                module test_one_func_one_tc {
                    function fun()
                    {
                    }
                    testcase tc()
                    {
                    }
                }
                ''')
        self.assertEqual(2, len(result))
        self.assertEqual("fun", result[0].name)
        self.assertEqual("__testcase__tc", result[1].name)
        self.assertEqual(3, result[0].start_line)
        self.assertEqual(5, result[0].end_line)
        self.assertEqual(6, result[1].start_line)
        self.assertEqual(8, result[1].end_line)

    def test_function_with_content(self):
        result = get_ttcn_function_list('''
                module test_func_w_ctx {
                    function fun(inout integer i)
                    {
                        i := call(i);
                    }
                }''')
        self.assertEqual(1, len(result))
        self.assertEqual("fun", result[0].name)
        self.assertEqual("fun( inout integer i )", result[0].long_name)

    def test_testcase_with_content(self):
        result = get_ttcn_function_list('''
                module test_tc_w_ctx {
                    testcase tc(integer i)
                    {
                        i := call(i);
                    }
                }''')
        self.assertEqual(1, len(result))
        self.assertEqual("__testcase__tc", result[0].name)
        self.assertEqual("__testcase__tc( integer i )", result[0].long_name)

    def test_function_with_runs_on(self):
        result = get_ttcn_function_list('''
                module test_function_with_runs_on {
                    function fun(inout integer i)
                    runs on MTC_CT
                    {
                        i := call(i);
                    }
                }''')
        self.assertEqual(1, len(result))
        self.assertEqual("fun", result[0].name)
        self.assertEqual("fun( inout integer i ) runs on MTC_CT",
                         result[0].long_name)

    def test_function_with_mtc(self):
        result = get_ttcn_function_list('''
                module test_function_with_mtc {
                    function fun( integer i )
                    mtc MTC_CT
                    {
                    }
                }''')
        self.assertEqual(1, len(result))
        self.assertEqual("fun", result[0].name)
        self.assertEqual("fun( integer i ) mtc MTC_CT", result[0].long_name)

    def test_function_with_system(self):
        result = get_ttcn_function_list('''
                module test_function_with_system {
                    function fun( integer i )
                    system MySystemType
                    {
                        var MySystemType v_st := system;
                    }
                }''')
        self.assertEqual(1, len(result))
        self.assertEqual("fun", result[0].name)
        self.assertEqual("fun( integer i ) system MySystemType",
                         result[0].long_name)

    def test_function_with_return(self):
        result = get_ttcn_function_list('''
                module test_function_with_return {
                    function fun( integer i )
                    return integer
                    {
                        return 42;
                    }
                }''')
        self.assertEqual(1, len(result))
        self.assertEqual("fun", result[0].name)
        self.assertEqual("fun( integer i ) return integer",
                         result[0].long_name)

    def test_function_with_return_w_template(self):
        result = get_ttcn_function_list('''
                module test_function_with_return_w_template {
                    function fun( integer i )
                    return template integer
                    {
                        return ?;
                    }
                }''')
        self.assertEqual(1, len(result))
        self.assertEqual("fun", result[0].name)
        self.assertEqual("fun( integer i ) return template integer",
                         result[0].long_name)

    def test_function_in_group(self):
        result = get_ttcn_function_list('''
                module test_function_in_group {
                    group g1 {
                        function fun( integer i )
                        {
                            return 0;
                        }
                    }
                }''')
        self.assertEqual(1, len(result))
        self.assertEqual("fun", result[0].name)
        self.assertEqual("fun( integer i )", result[0].long_name)

    def test_double_slash_within_string(self):
        result = get_ttcn_function_list('''
                module test_double_slash_within_string {
                    function fun()
                    {
                        character a := "\\";
                    }
                }''')
        self.assertEqual(1, len(result))

    def test_function_with_no_param(self):
        result = get_ttcn_function_list('''
                module test_function_with_no_param {
                    function fun()
                    return integer
                    {
                    }
                }''')
        self.assertEqual(0, result[0].parameter_count)

    def test_function_with_1_param(self):
        result = get_ttcn_function_list('''
                module test_function_with_1_param {
                    function fun(boolean b)
                    return integer
                    {
                        return 1;
                    }
                }''')
        self.assertEqual(1, result[0].parameter_count)

    def test_function_with_param(self):
        result = get_ttcn_function_list('''
                module test_function_with_param {
                    function fun(integer i := bb * cc, integer j := cc * dd)
                    return integer
                    {
                        return i * j;
                    }
                }''')
        self.assertEqual(2, result[0].parameter_count)

    def test_function_with_strang_param(self):
        result = get_ttcn_function_list('''
                module test_function_with_strang_param {
                    function fun(boolean b := aa < cc, boolean c := ee < dd)
                    return boolean
                    {
                        return b && c;
                    }
                }''')
        self.assertEqual(2, result[0].parameter_count)

    def test_function_with_template_param(self):
        result = get_ttcn_function_list('''
                module test_function_with_template_param {
                    function fun(out template boolean b)
                    {
                    }
                }''')
        self.assertEqual(1, len(result))
        self.assertEqual("fun", result[0].name)
        self.assertEqual("fun( out template boolean b )", result[0].long_name)

    def test_function_with_lazy_param(self):
        result = get_ttcn_function_list('''
                module test_function_with_lazy_param {
                    function fun(in @lazy integer p_i)
                    {
                    }
                }''')
        self.assertEqual(1, len(result))
        self.assertEqual("fun", result[0].name)
        self.assertEqual("fun( in @lazy integer p_i )", result[0].long_name)

    def test_function_with_fuzzy_param(self):
        result = get_ttcn_function_list('''
                module test_function_with_fuzzy_param {
                    function fun(in @fuzzy integer p_i)
                    {
                    }
                }''')
        self.assertEqual(1, len(result))
        self.assertEqual("fun", result[0].name)
        self.assertEqual("fun( in @fuzzy integer p_i )", result[0].long_name)

    def test_function_deterministic(self):
        result = get_ttcn_function_list('''
                module test_function_deterministic {
                    function @deterministic fun()
                    {
                    }
                }''')
        self.assertEqual(1, len(result))
        self.assertEqual("fun", result[0].name)
        self.assertEqual("@deterministic fun( )", result[0].long_name)

    def test_brakets_before_function(self):
        result = get_ttcn_function_list('''module err{\n()\n}''')
        self.assertEqual(0, len(result))

    def test_underscore(self):
        result = get_ttcn_function_list('''module err{\nfunction _(){}\n}''')
        self.assertEqual(0, len(result))

    def test_digits(self):
        result = get_ttcn_function_list('''module err{\nfunction 42(){}\n}''')
        self.assertEqual(0, len(result))

    def test_template(self):
        result = get_ttcn_function_list('''
                module test_template {
                    template MyMessageType MyTemplate1 (
                        template ( omit ) integer MyFormalParam
                    ):=
                    {
                        field1 := MyFormalParam,
                        field2 := pattern "abc*xyz",
                        field3 := true
                    }
                }''')
        self.assertEqual(0, len(result))


class Test_Preprocessing(unittest.TestCase):

    def test_content_macro_should_be_ignored(self):
        result = get_ttcn_function_list('''
                module test_content_macro_should_be_ignored {
                    #define MTP_CHEC                    \
                       function foo () {                     \
                       }
                } ''')
        self.assertEqual(0, len(result))

    def test_preproc_should_be_ignored_outside_func_impl(self):
        result = get_ttcn_function_list('''
                module test_preproc_should_be_ignored_outside_func_impl {
                    #ifdef MAGIC
                    #endif
                    function foo()
                    {}
                } ''')
        self.assertEqual(1, len(result))

    def test_preprocessor_is_not_function(self):
        result = get_ttcn_function_list('''
                module test_preprocessor_is_not_function {
                    #ifdef A
                    #elif (defined E)
                    #endif
                } ''')
        self.assertEqual(0, len(result))
