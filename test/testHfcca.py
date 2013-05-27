#
# Unit Test
#
import unittest
from hfcca import DefaultPreprocessor, FileAnalyzer, UniversalCodeCounter, SDLTokenTranslator, generate_tokens, ObjCTokenTranslator, generate_tokens_from_code, CTokenTranslator, mapFilesToAnalyzer, FunctionInfo

class Test_generate_tonken(unittest.TestCase):

    def test_empty_string(self):
        result = [t for t in generate_tokens_from_code("")]
        self.assertEqual(0, len(result))

    def test_with_one_return(self):
        result = [t for t in generate_tokens_from_code("\n")]
        self.assertEqual(1, len(result))

    def test_with_two_returns(self):
        result = [t for t in generate_tokens_from_code("\n\n")]
        self.assertEqual(1, len(result))

    def test_comment_count_as_a_token_when_generating(self):
        result = [t for t in generate_tokens_from_code("/**/")]
        self.assertEqual(1, len(result))

class Test_sdl_hfcca(unittest.TestCase):
    def create_sdl_hfcca(self, source_code):
        return UniversalCodeCounter(SDLTokenTranslator(generate_tokens(source_code)) , "").countCode()
    def test_empty(self):
        result = self.create_sdl_hfcca("")
        self.assertEqual(0, len(result))
    def test_process(self):
        result = self.create_sdl_hfcca("PROCESS pofcap\n ENDPROCESS;")
        self.assertEqual(1, len(result))
        self.assertTrue('PROCESS pofcap' in result)
    def test_one_function(self):
        result = self.create_sdl_hfcca("PROCEDURE xxx\n ENDPROCEDURE;");
        self.assertEqual(1, len(result))
        self.assertTrue("PROCEDURE xxx" in result)
        self.assertEqual(1, result[0].cyclomatic_complexity)
        self.assertEqual(0, result[0].token_count)
    def test_one_function_with_condition(self):
        result = self.create_sdl_hfcca(example_sdl_procedure);
        self.assertEqual(1, len(result))
        self.assertTrue("PROCEDURE send_swo_msgs__r" in result)
        self.assertEqual(7, result[0].cyclomatic_complexity)
        self.assertEqual(173, result[0].token_count)
    def test_one_function_with_array(self):
        result = self.create_sdl_hfcca("""
        PROCEDURE send_swo_msgs__r;
        START;
            TASK array(0):= 1;
        ENDPROCEDURE;
        """);
        self.assertEqual(1, len(result))
        self.assertEqual(1, result[0].cyclomatic_complexity)
    def test_process_with_content(self):
        result = self.create_sdl_hfcca(example_sdl_process);
        self.assertEqual(5, len(result))
        self.assertTrue("PROCEDURE send_swo_msgs__r" in result)
        self.assertTrue("PROCESS pofsrt" in result)
        self.assertTrue("PROCESS pofsrt STATE start_state INPUT supervision_msg_s" in result)
        self.assertTrue("PROCESS pofsrt STATE start_state1 INPUT supervision_msg_s2" in result)
        self.assertEqual(2, result[1].cyclomatic_complexity)

class Test_objc_hfcca(unittest.TestCase):
    def create_objc_hfcca(self, source_code):
        return UniversalCodeCounter(ObjCTokenTranslator(generate_tokens(source_code)) , "").countCode()
    def test_empty(self):
        result = self.create_objc_hfcca("")
        self.assertEqual(0, len(result))
    def test_no_function(self):
        result = self.create_objc_hfcca("#import <unistd.h>\n")
        self.assertEqual(0, len(result))
    def test_one_c_function(self):
        result = self.create_objc_hfcca("int fun(int a, int b) const{}")
        self.assertTrue("fun" in result)
    def test_one_objc_function(self):
        result = self.create_objc_hfcca("-(void) foo {}")
        self.assertTrue("foo" in result)
    def test_one_objc_function_with_param(self):
        result = self.create_objc_hfcca("-(void) replaceScene: (CCScene*) scene {}")
        self.assertEqual("replaceScene:", result[0].name)
        self.assertEqual("replaceScene:( CCScene * )", result[0].long_name())
    def test_one_objc_functio_nwith_two_param(self):
        result = self.create_objc_hfcca("- (BOOL)scanJSONObject:(id *)outObject error:(NSError **)outError {}")
        self.assertTrue("scanJSONObject: error:" in result)
        self.assertEqual("scanJSONObject:( id * ) error:( NSError ** )", result[0].long_name())
    def test_one_objc_function_with_three_param(self):
        result = self.create_objc_hfcca("- (id)initWithRequest:(NSURLRequest *)request delegate:(id <NSURLConnectionDelegate>)delegate startImmediately:(BOOL)startImmediately{}")
        self.assertEqual("initWithRequest: delegate: startImmediately:", result[0].name)
        self.assertEqual("initWithRequest:( NSURLRequest * ) delegate:( id < NSURLConnectionDelegate > ) startImmediately:( BOOL )", result[0].long_name())
    def test_implementation(self):
        code = """
            @implementation classname(xx)
            + (return_type)classMethod
            {
                if (failure){

                     //wefailed

                 }
            }
            - (return_type)instanceMethod
            {
                // implementation
            }
            @end
            """
        result = self.create_objc_hfcca(code)
        self.assertEqual(2, len(result))
        self.assertEqual("classMethod", result[0].name)
        


class Test_parser_token(unittest.TestCase):
    def get_tokens(self, source_code):
        return [x for x, l in generate_tokens(source_code)]
    def get_tokens_and_line(self, source_code):
        return [x for x in generate_tokens(source_code)]
    def test_empty(self):
        tokens = self.get_tokens("")
        self.assertEqual(0, len(tokens))
    def test_one_digit(self):
        tokens = self.get_tokens("1")
        self.assertEqual(['1'], tokens)
    def test_operators(self):
        tokens = self.get_tokens("-;")
        self.assertEqual(['-', ';'], tokens)
    def test_operators1(self):
        tokens = self.get_tokens("-=")
        self.assertEqual(['-='], tokens)
    def test_operators2(self):
        tokens = self.get_tokens(">=")
        self.assertEqual(['>='], tokens)

    def test_more(self):
        tokens = self.get_tokens("int a{}")
        self.assertEqual(['int', "a", "{", "}"], tokens)
    def test_or(self):
        tokens = self.get_tokens("||")
        self.assertEqual(['||'], tokens)
    def test_comment(self):
        tokens = self.get_tokens("/***\n**/")
        self.assertEqual([], tokens)
        tokens = self.get_tokens("//aaa\n")
        self.assertEqual(['\n'], tokens)
    def test_commentedComment(self):
        tokens = self.get_tokens(" /*/*/")
        self.assertEqual([], tokens)
    def test_string(self):
        tokens = self.get_tokens(r'""')
        self.assertEqual(['""'], tokens)
        tokens = self.get_tokens(r'"x\"xx")')
        self.assertEqual(['"x\\"xx"', ')'], tokens)
    def test_define(self):
        tokens = self.get_tokens('''#define xx()\
                                      abc
                                    int''')
        self.assertEqual(['\n', 'int'], tokens)
       
    def test_line_number(self):
        tokens = self.get_tokens_and_line(r'abc')
        self.assertEqual(('abc', 1), tokens[0])
    def test_line_number2(self):
        tokens = self.get_tokens_and_line('abc\ndef')
        self.assertTrue(('def', 2) in tokens)
    def test_with_mutiple_line_string(self):
        tokens = self.get_tokens_and_line('"sss\nsss" t')
        self.assertTrue(('t', 2) in tokens)
    def test_with_cpp_comments(self):
        tokens = self.get_tokens_and_line('//abc\n t')
        self.assertTrue(('t', 2) in tokens)
    def test_with_line_continuer_comments(self):
        tokens = self.get_tokens_and_line('#define a \\\nb\n t')
        self.assertTrue(('t', 3) in tokens)
    def test_define2(self):
        tokens = self.get_tokens_and_line(r''' # define yyMakeArray(ptr, count, size)     { MakeArray (ptr, count, size); \
                       yyCheckMemory (* ptr); }
                       t
''')
        self.assertTrue(('t', 3) in tokens)
    def test_with_c_comments(self):
        tokens = self.get_tokens_and_line('/*abc\n*/ t')
        self.assertTrue(('t', 2) in tokens)

class MockFileAnalyzer(FileAnalyzer):
    def __init__(self):
        self.mockRecord = []
    def analyze(self, filename):
        return filename
class Test_analyze_files(unittest.TestCase):
    def test_NoFiles(self):
        analyzer = MockFileAnalyzer()
        mapFilesToAnalyzer([], analyzer, 1)
        self.assertEqual(0, len(analyzer.mockRecord))
    def test_NoFilesMultipleThread(self):
        analyzer = MockFileAnalyzer()
        mapFilesToAnalyzer([], analyzer, 2)
        self.assertEqual(0, len(analyzer.mockRecord))
    def test_OneFile(self):
        analyzer = MockFileAnalyzer()
        r = mapFilesToAnalyzer(["filename"], analyzer, 1)
        self.assertEqual(["filename"], [x for x in r])
    def test_OneFileMultipleThread(self):
        analyzer = MockFileAnalyzer()
        r = mapFilesToAnalyzer(["filename"], analyzer, 2)
        self.assertEqual(["filename"], [x for x in r])
    def test_MoreFiles(self):
        analyzer = MockFileAnalyzer()
        r = mapFilesToAnalyzer(["f1", "f2"], analyzer, 1)
        self.assertEqual(["f1", "f2"], [x for x in r])
    def test_MoreFilesMultipleThread(self):
        analyzer = MockFileAnalyzer()
        r = mapFilesToAnalyzer(["f1", "f2"], analyzer, 2)
        self.assertEqual(["f1", "f2"], [x for x in r])

class MockFile:
    def __init__(self):
        self.mockRecord = []
    def read(self):
        return "int foo(){}"
    def close(self):
        pass
def mockOpen(name):
    return MockFile()
class Test_FileAnalyzer(unittest.TestCase):
    def setUp(self):
        self.analyzer = FileAnalyzer()
        self.analyzer.open = mockOpen
    def create_c_hfcca(self, source_code, preprocessor=DefaultPreprocessor):
        return FileAnalyzer().analyze_source_code_with_parser(source_code, preprocessor, "", CTokenTranslator)
    def test_analyze_c_file(self):
        r = mapFilesToAnalyzer(["f1.c"], self.analyzer, 1)
        self.assertEqual(1, len([x for x in r]))
    def test_analyze_c_file_with_multiple_thread(self):
        r = mapFilesToAnalyzer(["f1.c"], self.analyzer, 2)
        self.assertEqual(1, len([x for x in r]))
class Test_FunctionInfo(unittest.TestCase):
    def test_FunctionInfo_ShouldBePicklable(self):
        import pickle
        pickle.dumps(FunctionInfo("a", 1))

example_sdl_procedure = '''
/**************************************************************************/
PROCEDURE send_swo_msgs__r;
/*
 * Send the given switchover message to POFFIC in all computers in the target list.
 **************************************************************************/
FPAR
    IN/OUT  targets  targets__t,
    IN      msg_num  message_number_t;

DCL
    i     dword := 0,
    c_i   dword,
    msg_group message_group_t,
    activity_signal byte := msg_attr_t_normal_priority_c,
    ppid  pid;

START;
    DECISION routing_state__pv;
    ( routing_state_t_active_c ):
       TASK activity_signal := msg_attr_t_is_active__c;
    ENDDECISION;

    TASK  ppid := SELF;
    WHILE i < targets.item_count;
       TASK  set_pid_computer_r( ppid, targets.target(i).addr );
       TASK  c_i := 0,
             msg_group := direct_delivery_gi;

       WHILE c_i < 2;
          DECISION targets.target(i).chan(c_i);
          ( T ):
             DECISION msg_num;
             ( NUMBER_FROM( pof_deny_messages_s )):
                OUTPUT pof_deny_messages_s TO ppid,
                       SET GROUP = msg_group;
             ( NUMBER_FROM( pof_allow_messages_s )):
                OUTPUT pof_allow_messages_s TO ppid,
                       SET GROUP = msg_group, PRIORITY = activity_signal;
             ENDDECISION;
          ENDDECISION;
          TASK c_i := c_i + 1,
               msg_group := rt_direct_delivery_gi;
       ENDWHILE;

       TASK i := i + 1;
    ENDWHILE;
ENDPROCEDURE send_swo_msgs__r;
'''

example_sdl_process = r'''
PROCESS pofsrt
  COMMENT '@(#)SID: POFSRTGX.SDL 2.1-0 06/07/11';
/*
 */

DCL
  addr_range addr_range_t;

PROCEDURE send_swo_msgs__r;
START;
    TASK  ppid := SELF;
ENDPROCEDURE send_swo_msgs__r;
START;
  /* announce to DMXRTE */
  DECISION post_office_announcement_r( post_district_index_t_atm_c,
           addr_range,
           pof_advisable_msg_len__c - sizeof(buffer_bottom_t),
           pof_ack_waiting_time__c  );
    ( success_ec ):
      TASK /* nop */;
    ELSE:
      TASK /* hanskat tiskiin */;
  ENDDECISION;
  NEXTSTATE start_state
    COMMENT 'Nuthin fancy here';

/******************************************************/
STATE start_state
  COMMENT 'Wait for the first supervision message';

  INPUT supervision_msg_s(*);
    OUTPUT supervision_ack_s( INPUT ) TO SENDER;
    TASK pofsrt__r; /* call the actual code */
    NEXTSTATE -; /* this is actually never reached */
ENDSTATE start_state
STATE start_state1
  COMMENT 'Wait for the first supervision message';

#if (tr)
  INPUT supervision_msg_s1(*);
    OUTPUT supervision_ack_s( INPUT ) TO SENDER;
    TASK pofsrt__r; /* call the actual code */
    NEXTSTATE -; /* this is actually never reached */
#endif
  INPUT INTERNAL supervision_msg_s2(*);
    OUTPUT supervision_ack_s( INPUT ) TO SENDER;
    TASK pofsrt__r; /* call the actual code */
    NEXTSTATE -; /* this is actually never reached */
ENDSTATE start_state
  COMMENT 'Hand prefix started';
ENDPROCESS pofsrt;
'''

