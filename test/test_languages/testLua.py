import unittest
from lizard import analyze_file, FileAnalyzer, get_extensions
from lizard_languages import LuaReader


def get_function_list(source_code):
    return analyze_file.analyze_source_code(
        "a.lua", source_code).function_list

class Test_tokenizing_Lua(unittest.TestCase):

    def check_tokens(self, expect, source):
        tokens = list(LuaReader.generate_tokens(source))
        self.assertEqual(expect, tokens)

    def test_should_not_confuse_division_as_regx(self):
        self.check_tokens(['a','/','b',',','a','/','b'], 'a/b,a/b')
        self.check_tokens(['3453',' ','/','b',',','a','/','b'], '3453 /b,a/b')

    def test_double_square_brackets_string(self):
        self.check_tokens(['[[this is a string]]'], '[[this is a string]]')
        self.check_tokens(['[==[this is a string]==]'], '[==[this is a string]==]')

    def test_comment(self):
        self.check_tokens(['a', ' ', '--this is a comment', '\n'], "a --this is a comment\n")

    def test_comment_block(self):
        comment = '''--[[ multiple
        line
        comment]]'''
        self.check_tokens([comment, "\n"], comment + "\n")



class Test_parser_for_Lua(unittest.TestCase):

    def test_empty(self):
        functions = get_function_list("")
        self.assertEqual(0, len(functions))

    def test_no_function(self):
        result = get_function_list(''' p "1" ''')
        self.assertEqual(0, len(result))

    def test_one_function(self):
        result = get_function_list('''
            function f
            end
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual("f", result[0].name)
        self.assertEqual(0, result[0].parameter_count)
        self.assertEqual(1, result[0].cyclomatic_complexity)
        self.assertEqual(2, result[0].length)
        self.assertEqual(2, result[0].token_count)


    def test_comment_is_omitted_in_token_cound(self):
        result = get_function_list('''
            function f
            -- this is a comment
            end
                ''')
        self.assertEqual(2, result[0].token_count)


    def test_one_function_loc(self):
        result = get_function_list('''
            function f
                something
            end
                ''')
        self.assertEqual(3, result[0].length)
        self.assertEqual(3, result[0].nloc)

    def test_two_functions(self):
        result = get_function_list('''
            function f
            end
            function g
            end
                ''')
        self.assertEqual(2, len(result))
        self.assertEqual("g", result[1].name)

    def test_do(self):
        result = get_function_list('''
            function k
            end
            do
                function g
                end
            end
            function f
            end
                ''')
        self.assertEqual(3, len(result))

    def test_for_and_while(self):
        result = get_function_list('''
        function factorial(n)
          local x = 1
          for i = 2, n do
            x = x * i
          end
          while a do
            a=a-1
          end
          return x
        end
        function g
        end
                ''')
        self.assertEqual(2, len(result))
        self.assertEqual(3, result[0].cyclomatic_complexity)

    def test_ruby_block_words_are_lua_identifiers(self):
        result = get_function_list('''
local function page_count(record_count, module)
  return math.ceil(record_count / module.page_size)
end

local function create_frame(class, options)
  return class.new(options)
end

local function start_session(begin, name)
  return begin(name)
end
''')
        self.assertEqual(
            [('page_count', 2, 4), ('create_frame', 6, 8), ('start_session', 10, 12)],
            [(f.name, f.start_line, f.end_line) for f in result])

    def test_class_field_in_a_test_callback_preserves_the_enclosing_function(self):
        result = get_function_list('''
local function register_dns_tests()
  it("returns a DNS record", function()
    return { class = 1 }
  end)
  register_cache_tests()
end
''')
        self.assertEqual(
            [('(anonymous)', 3, 5), ('register_dns_tests', 2, 7)],
            [(f.name, f.start_line, f.end_line) for f in result])

    def test_repeat_until(self):
        result = get_function_list('''
        function f(n)
            repeat
              --statements
            until condition
        end
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_if(self):
        result = get_function_list('''
        function a(n)
            if a then
            elseif b then
            else
            end
        end
        function a(n)
        end
                ''')
        self.assertEqual(2, len(result))
        self.assertEqual(3, result[0].cyclomatic_complexity)

    def test_class_method(self):
        result = get_function_list('''
        function V.f(n)
        end
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual('V.f', result[0].name)

    def test_colon_method_keeps_its_qualified_name_and_parameters(self):
        result = get_function_list('''
function storage.pool:checkout(timeout, observer)
  return self:allocate(timeout, observer)
end
''')
        self.assertEqual(
            [('storage.pool:checkout', ['timeout', 'observer'])],
            [(f.name, f.parameters) for f in result])

    def test_colon_method_preserves_a_parenthesized_first_statement(self):
        result = get_function_list('''
function writer:write(message)
  (self.output or io.stdout):write(message)
end
function writer:with_callback()
  (function() print(self.output) end)()
end
''')
        self.assertEqual(
            [('writer:write', 2, 4, ['message']),
             ('(anonymous)', 6, 6, []),
             ('writer:with_callback', 5, 7, [])],
            [(f.name, f.start_line, f.end_line, f.parameters) for f in result])

    def test_anonymous(self):
        result = get_function_list('''
            function(self, neigh, id)
            end
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual('(anonymous)', result[0].name)

    def test_anonymous_with_assignment(self):
        result = get_function_list('''
            a = function(self, neigh, id)
            end
                ''')
        self.assertEqual(1, len(result))
        self.assertEqual('a', result[0].name)

    def test_nested_functions(self):
        result = get_function_list('''
        function addn(x)
          function sum(y)
            return x+y
          end
          return sum
        end
                ''')
        self.assertEqual(2, len(result))
        self.assertEqual('sum', result[0].name)
        self.assertEqual('addn', result[1].name)

    def test_it_is_a_regular_callback_inside_lua_functions(self):
        result = get_function_list('''
local function visit(items, it)
  for _, item in ipairs(items) do
    it(item)
  end
end
local function sample()
  return 1
end
''')
        self.assertEqual(
            [('visit', 2, 6), ('sample', 7, 9)],
            [(f.name, f.start_line, f.end_line) for f in result])

    def test_identifier_named_it_is_no_rspec_example(self):
        result = get_function_list('''
local it = 1
function f(t)
  t.it(it)
  return it
end
function g()
  return 2
end
''')
        self.assertEqual(
            [('f', 3, 6), ('g', 7, 9)],
            [(f.name, f.start_line, f.end_line) for f in result])

    def test_parenthesized_call_starts_the_body_after_parameters(self):
        result = get_function_list('''
function writer.flush()
  (writer.output or io.stdout):flush()
end

local function dispatch(callback)
  (function() return callback() end)()
end
''')
        self.assertEqual(
            [('writer.flush', 2, 4, []),
             ('(anonymous)', 7, 7, []),
             ('dispatch', 6, 8, ['callback'])],
            [(f.name, f.start_line, f.end_line, f.parameters) for f in result])

    def test_parenthesized_call_uses_a_local_fallback(self):
        result = get_function_list('''
local module = io.stdout
function writer.flush()
  (writer.output or module):flush()
end
''')
        self.assertEqual(
            [('writer.flush', 3, 5, [])],
            [(f.name, f.start_line, f.end_line, f.parameters) for f in result])

    def test_parameters_on_the_line_after_the_name(self):
        result = get_function_list('''
function f
(x)
  return x
end
''')
        self.assertEqual(
            [('f', 2, 5, 1)],
            [(f.name, f.start_line, f.end_line, f.parameter_count)
             for f in result])

    def test_call_of_it_in_a_loop_with_a_length(self):
        result = get_function_list('''
local function f(t, it)
  for i = 1, #t do
    it()
  end
end
local function g()
  return 1
end
''')
        self.assertEqual(
            [('f', 2, 6), ('g', 7, 9)],
            [(f.name, f.start_line, f.end_line) for f in result])
