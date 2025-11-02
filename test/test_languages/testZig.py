import unittest

from lizard import analyze_file


def get_zig_fileinfo(source_code):
    return analyze_file.analyze_source_code("a.zig", source_code)


def get_zig_function_list(source_code):
    return get_zig_fileinfo(source_code).function_list


class TestZig(unittest.TestCase):
    def test_empty(self):
        functions = get_zig_function_list("")
        self.assertEqual(0, len(functions))

    def test_one_function(self):
        result = get_zig_function_list("""
            pub fn sayGoodbye() void { }
        """)
        self.assertEqual(1, len(result))
        self.assertEqual("sayGoodbye", result[0].name)
        self.assertEqual(0, result[0].parameter_count)
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_one_with_parameter(self):
        result = get_zig_function_list("""
            pub fn sayGoodbye(personName: string, alreadyGreeted: bool) void { }
        """)
        self.assertEqual(1, len(result))
        self.assertEqual("sayGoodbye", result[0].name)
        self.assertEqual(2, result[0].parameter_count)

    def test_one_function_with_return_value(self):
        result = get_zig_function_list("""
            fn sayGoodbye() []const u8 { }
        """)
        self.assertEqual(1, len(result))
        self.assertEqual("sayGoodbye", result[0].name)

    def test_one_function_with_tuple_return_value(self):
        result = get_zig_function_list("""
           fn sayGoodbye(p: int) struct {[]const u8, bool} { }
        """)
        self.assertEqual(1, len(result))
        self.assertEqual("sayGoodbye", result[0].name)
        self.assertEqual(1, result[0].parameter_count)

    def test_one_function_defined_on_a_struct(self):
        result = get_zig_function_list("""
            const Vec3 = struct {
                x: f32,
                y: f32,
                z: f32,

                pub fn dot(self: Vec3, other: Vec3) f32 {
                    return self.x * other.x + self.y * other.y + self.z * other.z;
                }
            };
        """)
        self.assertEqual(1, len(result))
        self.assertEqual("dot", result[0].name)
        self.assertEqual("dot self : Vec3 , other : Vec3", result[0].long_name)

    def test_one_function_with_complexity(self):
        result = get_zig_function_list("""
            fn sayGoodbye() void { if ((diceRoll + 1) == 7) { diceRoll = 1 }}
        """)
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_nest_function(self):
        result = get_zig_function_list("""
            fn foo() fn() u32 {
                const x: u32 = 123;

                const T = struct {
                    fn bar() u32 { return x; }
                    fn baz(n: int) u32 { return x + 1; }
                };

                const T2 = struct {
                    fn bar() u32 {
                        return x;
                    }
                };

                return T.bar;
            }
        """)
        self.assertEqual(4, len(result))

        self.assertEqual("bar", result[0].name)
        self.assertEqual("bar", result[0].long_name)
        self.assertEqual(1, result[0].length)

        self.assertEqual("baz", result[1].name)
        self.assertEqual("baz n : int", result[1].long_name)
        self.assertEqual(1, result[1].length)
        self.assertEqual(["n : int"], result[1].full_parameters)

        self.assertEqual("bar", result[2].name)
        self.assertEqual("bar", result[2].long_name)
        self.assertEqual(3, result[2].length)

        self.assertEqual("foo", result[3].name)
        self.assertEqual(16, result[3].length)

    def test_struct(self):
        result = get_zig_function_list("""
            const Point = struct {
                x: f32,
                y: f32,
            };
            fn sayGoodbye() { }
        """)
        self.assertEqual(1, len(result))
        self.assertEqual("sayGoodbye", result[0].name)

    def test_interface_followed_by_enum_and_union(self):
        result = get_zig_function_list("""
            const Point = struct {
                x: f32,
                y: f32,
            };
            const Type = enum {
                Ok,
                NotOk,
            };
            const Payload = union {
                Int: i64,
                Float: f64,
                Bool: bool,
            };
        """)
        self.assertEqual(0, len(result))

    def test_function_with_one(self):
        result = get_zig_function_list("""
            const std = @import("std");

            pub fn main() !void {
                std.debug.print("Hello, World!\n", .{});
            }
        """)
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_function_with_two(self):
        result = get_zig_function_list("""
            const std = @import("std");

            pub fn main() !void {
                if (condition) {
                    std.debug.print("Hello, World!\n", .{});
                }
            }
        """)
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_function_with_three(self):
        result = get_zig_function_list("""
            const std = @import("std");

            pub fn main() !void {
                if (condition1 or condition2) {
                    std.debug.print("Hello, World!\n", .{});
                }
            }
        """)
        self.assertEqual(3, result[0].cyclomatic_complexity)

    def test_function_with_eight(self):
        result = get_zig_function_list("""
            const std = @import("std");

            pub fn main() !void {
                switch (day) {
                    .Monday => std.debug.print("Today is Monday!\n", .{}),
                    .Tuesday => std.debug.print("Today is Tuesday!\n", .{}),
                    .Wednesday => std.debug.print("Today is Wednesday!\n", .{}),
                    .Thursday => std.debug.print("Today is Thursday!\n", .{}),
                    .Friday => std.debug.print("Today is Friday!\n", .{}),
                    .Saturday => std.debug.print("Today is Saturday!\n", .{}),
                    .Sunday => std.debug.print("Today is Sunday!\n", .{}),
                }
            }
        """)
        self.assertEqual(8, result[0].cyclomatic_complexity)

    def test_null_orelse_operator(self):
        result = get_zig_function_list("""
            pub fn main() !void {
                a orelse b;
            }
        """)
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_try_operator(self):
        result = get_zig_function_list("""
            pub fn main(str: []u8) !void {
                const number = try parseU64(str, 10);
                _ = number;
            }
        """)
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_catch_operator(self):
        result = get_zig_function_list("""
            pub fn main() !void {
                const value: anyerror!u32 = error.Broken;
                const unwrapped = value catch 1234;
                unwrapped == 1234
            }
        """)
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_for_loop(self):
        result = get_zig_function_list("""
            const std = @import("std");

            pub fn main() !void {
                for (0..5) {
                    std.debug.print("Hello, World!\n", .{});
                }
            }
        """)
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_while_loop(self):
        result = get_zig_function_list("""
            const std = @import("std");

            pub fn main() !void {
                var i: usize = 1;
                while (i < 10) {
                    std.debug.print("Hello, World!\n", .{});
                    i *= 2;
                }
            }
        """)
        self.assertEqual(2, result[0].cyclomatic_complexity)


class Test_Zig_Cognitive_Complexity(unittest.TestCase):
    """Cognitive Complexity tests for Zig"""

    def test_simple_function_has_zero_cogc(self):
        """Empty or simple straight-line function should have CogC=0"""
        code = '''
        pub fn simple(x: i32) i32 {
            return x + 1;
        }
        '''
        functions = get_zig_function_list(code)
        self.assertEqual(0, functions[0].cognitive_complexity)

    def test_single_if_statement(self):
        """Single if statement should be CogC=1"""
        code = '''
        pub fn check(x: i32) []const u8 {
            if (x > 0) {  // +1
                return "positive";
            }
            return "non-positive";
        }
        '''
        functions = get_zig_function_list(code)
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_nested_loops_with_nesting_penalty(self):
        """Nested structures demonstrate nesting penalty multiplication"""
        code = '''
        pub fn nested() void {
            var i: usize = 0;
            while (i < 10) : (i += 1) {           // +1
                var j: usize = 0;
                while (j < 10) : (j += 1) {       // +2 (nesting=1)
                    if (i == j) {                 // +3 (nesting=2)
                        // do something
                    }
                }
            }
        }
        // Total CogC = 6
        '''
        functions = get_zig_function_list(code)
        self.assertEqual(6, functions[0].cognitive_complexity)

    def test_binary_logical_operators(self):
        """Binary logical operator sequences"""
        code = '''
        pub fn check(a: bool, b: bool, c: bool, d: bool, e: bool) i32 {
            if (a and b and c) {  // +1 for if, +1 for 'and' sequence
                return 1;
            }
            if (d or e) {         // +1 for if, +1 for 'or' sequence
                return 2;
            }
            return 0;
        }
        // Total CogC = 4
        '''
        functions = get_zig_function_list(code)
        self.assertEqual(4, functions[0].cognitive_complexity)

    def test_switch_statement_counts_as_one(self):
        """Switch statement should count as +1 total"""
        code = '''
        pub fn classify(x: i32) []const u8 {
            return switch (x) {  // +1
                1 => "one",
                2 => "two",
                3 => "three",
                else => "other",
            };
        }
        // Total CogC = 1
        '''
        functions = get_zig_function_list(code)
        self.assertEqual(1, functions[0].cognitive_complexity)

    def test_while_loop(self):
        """While loop should count as +1"""
        code = '''
        pub fn countdown(n: i32) i32 {
            var x = n;
            while (x > 0) {  // +1
                x = x - 1;
            }
            return x;
        }
        // Total CogC = 1
        '''
        functions = get_zig_function_list(code)
        self.assertEqual(1, functions[0].cognitive_complexity)
