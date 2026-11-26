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

    def test_custom_token_backreference_keeps_its_capture_number(self):
        addition = r'|(\d+)-\1|\[[^\n]+?\]'
        source = 'return 12-12, [=[first ]] line]=], [==[second]=] line]==]'

        tokens = list(LuaReader.generate_tokens(source, addition))

        self.assertEqual(
            ['return', ' ', '12-12', ',', ' ', '[=[first ]] line]=]',
             ',', ' ', '[==[second]=] line]==]'],
            tokens)

    def test_verbose_custom_tokens_preserve_a_shebang(self):
        addition = r'(?x)| ([0-9]+) - \1'
        script = '#!/usr/bin/env lua\nreturn 12-12\n'
        expected = ['#!/usr/bin/env lua', '\n', 'return', ' ', '12-12', '\n']

        self.assertEqual(expected, list(LuaReader.generate_tokens(script, addition)))
        self.assertEqual(
            ['\ufeff'] + expected,
            list(LuaReader.generate_tokens('\ufeff' + script, addition)))

    def test_comment(self):
        self.check_tokens(['a', ' ', '--this is a comment', '\n'], "a --this is a comment\n")

    def test_comment_block(self):
        comment = '''--[[ multiple
        line
        comment]]'''
        self.check_tokens([comment, "\n"], comment + "\n")



class Test_parser_for_Lua(unittest.TestCase):

    def test_floor_division_keeps_the_rest_of_the_line(self):
        code = '''
local function pages(record_count, page_size)
  return record_count // page_size + (record_count % page_size > 0 and 1 or 0)
end
'''

        functions = get_function_list(code)

        self.assertEqual([('pages', 3)], [
            (fn.name, fn.cyclomatic_complexity) for fn in functions])

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

    def test_long_comment_with_equal_signs_does_not_count_as_code(self):
        result = get_function_list('''
local function scale(value)
  --[=[
  if config.old_scale then
    return value or 0
  end
  ]=]
  return value * 2
end
''')
        self.assertEqual(
            [('scale', 2, 9, 1, 3)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity, f.nloc)
             for f in result])

    def test_long_string_ignores_a_shorter_closing_delimiter(self):
        result = get_function_list('''
local function query_script()
  return [=[
  local last = values[indexes[i]]
  if last then
    return last
  end
  ]=]
end
''')
        self.assertEqual(
            [('query_script', 2, 9, 1)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
             for f in result])

    def test_nested_long_delimiters_preserve_the_following_callback(self):
        result = get_function_list('''
describe("jobs", function()
  it("waits for the child job", function()
    child_exec_lua([[
      ffi.cdef[=[ void start_job(void); ]=]
      start({timeout = 300})
    ]])
    retry(function()
      return child_ready()
    end)
  end)
end)
''')
        self.assertEqual(
            [(8, 10), (3, 11), (2, 12)],
            [(f.start_line, f.end_line) for f in result])

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

    def test_unless_is_a_regular_lua_callback(self):
        result = get_function_list('''
local function deliver(request, unless)
  unless(request)
end
''')
        self.assertEqual(
            [('deliver', 2, 4)],
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

    def test_assigned_lua_method_uses_the_qualified_target_name(self):
        source = '''
local function install(routes, handler)
  routes.main.handler = function(request)
    return handler(request)
  end
end
'''
        analyzer = FileAnalyzer(get_extensions(['cognitive']))
        result = analyzer.analyze_source_code('routes.lua', source)
        self.assertEqual(
            [('routes.main.handler', 0), ('install', 0)],
            [(f.name, f.cognitive_complexity) for f in result.function_list])

    def test_callback_call_to_an_outer_parameter_does_not_count_as_recursion(self):
        analyzer = FileAnalyzer(get_extensions(['cognitive']))
        result = analyzer.analyze_source_code('routes.lua', '''
local function install(routes, check, handler)
  prepare() if check(function() routes.main.handler
      = function(request)
        return handler(request)
      end
    end) then
    return true
  end
end
''')
        self.assertEqual(
            [0],
            [f.cognitive_complexity for f in result.function_list if f.start_line == 4])

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

    def test_length_operator_keeps_the_rest_of_the_expression(self):
        result = get_function_list('''
local function groups_per_team(group_count, team)
  return group_count / #team > 0 and 1 or 0
end
''')
        self.assertEqual(3, result[0].cyclomatic_complexity)

    def test_length_operator_keeps_the_loop_body(self):
        result = get_function_list('''
local function enable(accounts)
  for i = 1, #accounts do
    if accounts[i].active then
      accounts[i]:enable()
    end
  end
end
''')
        self.assertEqual(
            [('enable', 2, 8)],
            [(f.name, f.start_line, f.end_line) for f in result])

    def test_length_operator_in_a_wrapped_loop_header(self):
        result = get_function_list('''
local function enable(accounts)
  for i = 1,
      #accounts do
    if accounts[i].active then
      accounts[i]:enable()
    end
  end
end
''')
        self.assertEqual(
            [('enable', 2, 9)],
            [(f.name, f.start_line, f.end_line) for f in result])

    def test_length_and_floor_division_in_a_wrapped_loop_header(self):
        result = get_function_list('''
local function paginate(limit, payload, page_size)
  for page = 1, limit // page_size +
      #payload do
    send_page(payload, page)
  end
end
''')
        self.assertEqual(
            [('paginate', 2, 7, 2)],
            [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
             for f in result])

    def test_table_loop_header_keeps_the_enclosing_test_callback(self):
        result = get_function_list('''
describe('marks', function()
  it('opens a mark', function() open_mark() end)
  for _, entry in ipairs({
    { name = 'mark' },
  }) do
    eq({ entry.name }, { 'mark' })
  end
end)
''')
        self.assertEqual(
            [('(anonymous)', 3, 3), ('(anonymous)', 2, 9)],
            [(f.name, f.start_line, f.end_line) for f in result])

    def test_loop_header_keeps_the_callback_and_loop_body(self):
        result = get_function_list('''
local function visit(root)
  for path, kind in walk(root, {
    ignore = function(name)
      do
        return name == ".git"
      end
    end,
  }) do
    if kind == "file" then
      index(path)
    end
  end
end
''')
        self.assertEqual(
            [('ignore', 4, 8), ('visit', 2, 14)],
            [(f.name, f.start_line, f.end_line) for f in result])

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

    def test_if_and_while_can_follow_a_statement_on_the_same_line(self):
        source = '''
local function enable(account)
  account:refresh() if account.active
  then
    account:enable()
  end
end

local function drain(queue)
  queue:refresh() while queue.pending
  do
    queue:drain()
  end
end
'''
        analyzer = FileAnalyzer(get_extensions(['nd', 'ns']))
        result = analyzer.analyze_source_code('state.lua', source)
        self.assertEqual(
            [('enable', 2, 7, 1, 1), ('drain', 9, 14, 1, 1)],
            [(f.name, f.start_line, f.end_line, f.max_nesting_depth, f.max_nested_structures)
             for f in result.function_list])

    def test_nesting_across_lua_conditional_branches(self):
        source = '''
local function nested(cache)
  if cache.ready then
    return cache
  else
    local fallback = cache:load()
    if fallback then
      return fallback
    end
  end
end

local function siblings(cache)
  if cache.ready then
    cache:touch()
  elseif cache.expired then
    cache:renew()
  end
  if cache.dirty then
    cache:save()
  end
end
'''
        analyzer = FileAnalyzer(get_extensions(['nd', 'ns']))
        result = analyzer.analyze_source_code('cache.lua', source)
        self.assertEqual(
            [('nested', 2, 2), ('siblings', 1, 1)],
            [(f.name, f.max_nesting_depth, f.max_nested_structures)
             for f in result.function_list])

    def test_nesting_in_loop_bodies_and_table_constructors(self):
        source = '''
local function build_grid(size)
  local grid = {}
  for row = 1, size do
    grid[#grid + 1] = {}
    for col = 1, size do
      grid[row][col] = row + col
    end
  end
  return grid
end

local function visit(items, task)
  for i = 1, #items do
    task:catch(items[i].try)
  end
end
'''
        analyzer = FileAnalyzer(get_extensions(['nd', 'ns']))
        result = analyzer.analyze_source_code('grid.lua', source)
        self.assertEqual(
            [('build_grid', 2, 2), ('visit', 1, 1)],
            [(f.name, f.max_nesting_depth, f.max_nested_structures)
             for f in result.function_list])

    def test_repeat_loops_end_at_until(self):
        source = '''
local function send(socket)
  repeat
    if socket.ready then
      socket:send()
    end
  until not socket.retry
end

local function save(socket)
  repeat
    socket:send()
  until not socket.retry
  if socket.closed then
    socket:save()
  end
end
'''
        analyzer = FileAnalyzer(get_extensions(['nd', 'ns']))
        result = analyzer.analyze_source_code('socket.lua', source)
        self.assertEqual(
            [('send', 2, 8, 2, 2), ('save', 10, 17, 1, 1)],
            [(f.name, f.start_line, f.end_line, f.max_nesting_depth, f.max_nested_structures)
             for f in result.function_list])

    def test_standalone_do_and_inner_functions_have_their_own_scopes(self):
        source = '''
local function register(queue)
  if queue.ready then
    do
      local function handle(item)
        if item.valid then
          queue:accept(item)
        end
      end
      queue:subscribe(handle)
    end
  end
  while queue.pending do
    queue:drain()
  end
end
'''
        analyzer = FileAnalyzer(get_extensions(['nd', 'ns']))
        result = analyzer.analyze_source_code('queue.lua', source)
        self.assertEqual(
            [('handle', 1, 1), ('register', 1, 2)],
            [(f.name, f.max_nesting_depth, f.max_nested_structures)
             for f in result.function_list])

    def test_lua_method_names_do_not_add_nesting_depth(self):
        analyzer = FileAnalyzer(get_extensions(['nd']))
        result = analyzer.analyze_source_code('schedule.lua', '''
local function schedule(task, report_error)
  task.try(1)
  task:catch(report_error)
end
''')
        self.assertEqual([0], [f.max_nesting_depth for f in result.function_list])
