import unittest

from lizard import analyze_file


def get_zig_fileinfo(source_code):
    return analyze_file.analyze_source_code("a.zig", source_code)


def get_zig_function_list(source_code):
    return get_zig_fileinfo(source_code).function_list


class TestZig(unittest.TestCase):
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

    def test_null_coalescing_operator(self):
        result = get_zig_function_list("""
            pub fn main() !void {
                a orelse b;
            }
        """)
        self.assertEqual(2, result[0].cyclomatic_complexity)
