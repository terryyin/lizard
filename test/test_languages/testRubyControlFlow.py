import unittest
from lizard import FileAnalyzer, analyze_file, get_extensions


def get_ruby_function_list(source_code):
    return analyze_file.analyze_source_code(
        "a.rb", source_code).function_list


class Test_parser_for_Ruby_if_while_for(unittest.TestCase):
    def test_basic_if_block(self):
        result = get_ruby_function_list('''
            def f
                if a
                end
            end
                ''')
        self.assertEqual(4, result[0].nloc)

    def test_basic_if_oneliner_block(self):
        result = get_ruby_function_list('''
            def f; if a; end; end
                ''')
        self.assertEqual(1, result[0].nloc)

    def test_basic_if_modifier(self):
        result = get_ruby_function_list('''
            def f
                a if b
            end
                ''')
        self.assertEqual(3, result[0].nloc)

    def test_modifier_predicate_keeps_its_own_block(self):
        result = get_ruby_function_list('''
def dispatch(jobs, paused)
  return if jobs.any? do |job|
    job.pending?
  end
  dispatch_ready_jobs(jobs) if paused
end

def release(jobs)
  return unless jobs.synchronize do
    jobs.all?(&:finished?)
  end
  release_workers
end
''')

        self.assertEqual(
            [('dispatch', 2, 7), ('release', 9, 14)],
            [(function.name, function.start_line, function.end_line)
             for function in result])
        self.assertEqual(3, result[0].cyclomatic_complexity)

    def test_nested_structures_close_at_ruby_block_boundaries(self):
        source = '''
def render(ready, cached)
  if ready
    show_current
  end
  if cached
    show_saved
  end
end

def dispatch(jobs, paused)
  return if jobs.any? do |job|
    job.pending?
  end
  if paused
    notify_workers
  end
end

def release(ready, active)
  if ready
    if active
      release_workers
    end
  end
end
'''
        analyzer = FileAnalyzer(get_extensions(['ns']))

        functions = analyzer.analyze_source_code('dispatch.rb', source).function_list

        self.assertEqual(
            [('render', 1), ('dispatch', 2), ('release', 2)],
            [(function.name, function.max_nested_structures)
             for function in functions])

    def test_modifier_wraps_the_block_before_it(self):
        source = '''
def release(pool)
  pool.synchronize do
    if pool.idle?
      pool.release
    end
  end if pool.ready?
end
'''
        analyzer = FileAnalyzer(get_extensions(['ns']))

        function, = analyzer.analyze_source_code('pool.rb', source).function_list

        self.assertEqual(3, function.max_nested_structures)

    def test_modifier_wraps_the_complete_call_expression(self):
        source = '''
def rendered_jobs(jobs, ready)
  jobs.map do |job|
    job.render if job.active?
  end.compact if ready
end

def scheduled_jobs(jobs, ready)
  if ready then jobs.each do |job|
    job.run if job.active?
  end else warn('No jobs') if jobs.empty?
  end
end

def later_guard(jobs, ready)
  jobs.map do |job|
    job.render if job.active?
  end.compact
  log_jobs if ready
end
'''
        analyzer = FileAnalyzer(get_extensions(['ns']))

        functions = analyzer.analyze_source_code('jobs.rb', source).function_list

        self.assertEqual([('rendered_jobs', 3), ('scheduled_jobs', 3), ('later_guard', 2)],
                         [(function.name, function.max_nested_structures) for function in functions])

    def test_nesting_depth_uses_ruby_if_boundaries(self):
        source = '''
def render(ready, cached)
  if ready
    show_current
  end
  if cached
    show_saved
  end
end

def dispatch(jobs, paused, silent)
  if jobs.any? { |job| job.pending? }
    queue_jobs(jobs) if paused
    if silent
      record_dispatch
    end
  end
end

def release(ready, active)
  if ready
    release_workers
  else
    if active
      notify_workers
    end
  end
end
'''
        analyzer = FileAnalyzer(get_extensions(['nd']))

        functions = analyzer.analyze_source_code('dispatch.rb', source).function_list

        self.assertEqual(
            [('render', 1), ('dispatch', 2), ('release', 2)],
            [(function.name, function.max_nesting_depth)
             for function in functions])

    def test_logical_assignment_does_not_open_an_extra_nesting_level(self):
        source = '''
def store_path(options, path)
  if options[:gemspec]
    store_gemspec(path)
  else
    @global_path ||= path if options[:global]
  end
end

def load_cache(enabled)
  if enabled
    keys = {}
    keys[:name] ||= read_name
  end
end

def validate_cache(ready, expired)
  if ready || expired
    validate
  end
end
'''
        analyzer = FileAnalyzer(get_extensions(['nd']))

        functions = analyzer.analyze_source_code('cache.rb', source).function_list

        self.assertEqual(
            [('store_path', 2), ('load_cache', 1), ('validate_cache', 2)],
            [(function.name, function.max_nesting_depth) for function in functions])

    def test_loop_depth_covers_the_body_before_or_after_its_header(self):
        source = '''
def save_valid
  while (item = next_item)
    if item.valid?
      save(item)
    end
  end
end

def notify_valid
  while (item = next_item) do
    if item.valid?
      notify(item)
    end
  end
end

def cancel_all(items)
  for item in items
    if item.open?
      cancel(item)
    end
  end
end

def compare_batches(left, right)
  begin
    return true if left.empty? && right.empty?
  end while left == right
end

def dispatch_ready(queue)
  queue.each do |job|
    dispatch(job) if job.ready?
  end if queue.available?
end
'''
        analyzer = FileAnalyzer(get_extensions(['nd']))

        functions = analyzer.analyze_source_code('queue.rb', source).function_list

        self.assertEqual(
            [('save_valid', 2), ('notify_valid', 2), ('cancel_all', 2),
             ('compare_batches', 3), ('dispatch_ready', 2)],
            [(function.name, function.max_nesting_depth) for function in functions])

    def test_basic_if_with_then_on_one_line(self):
        result = get_ruby_function_list('''
            def f
                a = if b then c else d end
            end
                ''')
        self.assertEqual(3, result[0].nloc)

    def test_unless(self):
        result = get_ruby_function_list('''
            def f
                a = unless b then c else d end
            end
                ''')
        self.assertEqual(3, result[0].nloc)

    def test_basic_while_block(self):
        result = get_ruby_function_list('''
            def f
                while a
                end
                for a
                end
            end
                ''')
        self.assertEqual(6, result[0].nloc)

    def test_basic_while_modifier(self):
        result = get_ruby_function_list('''
            def f
                a while a
            end
                ''')
        self.assertEqual(3, result[0].nloc)

    def test_while_with_do(self):
        result = get_ruby_function_list('''
            def f
                while a do
                end
            end
                ''')
        self.assertEqual(4, result[0].nloc)

    def test_while_modifier_with_block(self):
        result = get_ruby_function_list('''
            def f
                begin
                end while a
            end
                ''')
        self.assertEqual(4, result[0].nloc)

    def test_class_as_an_symbol(self):
        result = get_ruby_function_list('''
            def f
                begin
                end while a
            end
                ''')
        self.assertEqual(4, result[0].nloc)


    def test_rspec_it(self):
        result = get_ruby_function_list('''
            describe 'xx' do
              it "does something" do
              end
              it "does something else" do
              end
              context "xx" do
                it "xxxx" do
                    xxx
                end
              end
            end
                ''')
        self.assertEqual(3, result[2].nloc)

    def test_rspec_it_with_brackets(self):
        result = get_ruby_function_list('''
            describe 'xx' do
              it { }
              it "does something else" do
                a if b
              end
              context "xx" do
                it "xxxx" do
                    xxx
                end
              end
            end
                ''')
        self.assertEqual(3, len(result))
        self.assertEqual(1, result[0].nloc)
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_it_as_variable(self):
        result = get_ruby_function_list('''
            describe 'xx' do
              it = 3
            end
                ''')
        self.assertEqual(0, len(result))


def test_postfix_guards_keep_the_depth_of_the_expression_they_wrap():
    source = '''
def rendered_jobs(jobs, ready)
  jobs.map do |job|
    job.render if job.active?
  end.compact if ready
end

def retry_jobs(left, right, enabled)
  begin
    return true if left.empty? && right.empty?
  end while left == right if enabled
end

def run_jobs(jobs, ready, enabled)
  jobs.each do |job|
    job.run if job.active?
  end if ready while enabled
end

def later_guard(jobs, ready)
  jobs.map do |job|
    job.render if job.active?
  end.compact
  log_jobs if ready
end

def grouped_jobs(jobs, ready)
  (jobs.each do |job|
    job.run if job.active?
  end) if ready
end
'''
    analyzer = FileAnalyzer(get_extensions(['nd']))

    functions = analyzer.analyze_source_code('jobs.rb', source).function_list

    assert [(f.name, f.max_nesting_depth) for f in functions] == [
        ('rendered_jobs', 2), ('retry_jobs', 4), ('run_jobs', 3),
        ('later_guard', 1), ('grouped_jobs', 2),
    ]


def test_postfix_guards_keep_independent_statements_at_the_same_depth():
    source = '''
def append_actions(key)
  columns = Array(key.column).map { |column| quote(column) }
  sql = +<<~SQL
    FOREIGN KEY (#{columns.join(", ")})
  SQL
  sql << " #{action_sql('DELETE', key.on_delete)}" if key.on_delete
  sql << " #{action_sql('UPDATE', key.on_update)}" if key.on_update
end

def schedule(jobs, ready)
  if ready then jobs.each do |job|
    job.run if job.active?
  end else warn('No jobs') if jobs.empty?
  end
end

def assign_layout_data(layout)
  payload[:layout] = merge(layout.data, payload[:layout] || {}) if layout
end

def association_type(options)
  type = -(options[:foreign_type]&.to_s || "#{options[:as]}_type") if options[:as]
end

'''
    analyzer = FileAnalyzer(get_extensions(['nd']))

    functions = analyzer.analyze_source_code('jobs.rb', source).function_list

    assert [(f.name, f.max_nesting_depth) for f in functions] == [
        ('append_actions', 1), ('schedule', 2), ('assign_layout_data', 2),
        ('association_type', 2),
    ]
