import unittest
from .testGo import get_go_function_list


class TestGoDeclarations(unittest.TestCase):

    def test_package_level_function_literal_is_named_by_var(self):
        result = get_go_function_list('''
            var handler = func(a int) { if a > 0 { } }
            func after() { }
                ''')
        self.assertEqual(["handler", "after"], [f.name for f in result])
        self.assertEqual(2, result[0].cyclomatic_complexity)
        self.assertEqual(1, result[0].parameter_count)

    def test_package_level_typed_function_var(self):
        result = get_go_function_list('''
            var handler func(int) error = func(a int) error { return nil }
            func after() { }
                ''')
        self.assertEqual(["handler", "after"], [f.name for f in result])

    def test_function_type_var_without_initializer(self):
        result = get_go_function_list('''
            var handler func(int) error
            func after() { }
                ''')
        self.assertEqual(["after"], [f.name for f in result])

    def test_function_literal_in_package_level_composite(self):
        result = get_go_function_list('''
            var table = []Handler{ func() { }, }
            func after() { }
                ''')
        self.assertEqual(["anonymous__0", "after"], [f.name for f in result])

    def test_bare_func_literal_does_not_swallow_following_functions(self):
        result = get_go_function_list('''
            var x = func
            func after() { }
                ''')
        self.assertEqual(["after"], [f.name for f in result])

    def test_type_switch_does_not_end_function_early(self):
        result = get_go_function_list('''
            func f(x interface{}) {
                switch v := x.(type) {
                case int:
                    if v > 0 { }
                }
                if true { }
            }
            func g() { }
                ''')
        self.assertEqual(["f", "g"], [f.name for f in result])
        self.assertEqual(4, result[0].cyclomatic_complexity)

    def test_func_type_in_composite_literal_type(self):
        result = get_go_function_list('''
            var table = map[string]func(){ "a": func() { if true { } }, }
            var list = []func() error{ func() error { return nil } }
            func after() {
                hs := make([]func(), 0)
                var c chan func()
                if true { }
            }
                ''')
        self.assertEqual(["anonymous__0", "anonymous__1", "after"],
                         [f.name for f in result])
        self.assertEqual(2, result[0].cyclomatic_complexity)
        self.assertEqual(2, result[2].cyclomatic_complexity)


def test_inline_result_struct_does_not_end_function_before_its_body():
    result = get_go_function_list('''package p
func valueReporter() func(int) struct{ Value int } {
    return func(value int) struct{ Value int } {
        return struct{ Value int }{value}
    }
}
func reset() {}
''')
    assert [(f.name, f.start_line, f.end_line) for f in result] == [
        ('anonymous__0', 3, 5), ('valueReporter', 2, 6), ('reset', 7, 7)]


def test_array_length_composite_literal_does_not_open_the_function_body():
    result = get_go_function_list('''package wire

import "encoding/binary"
import "unsafe"

type Header struct { Version, Flags uint32 }

func encodeHeader(header Header) [unsafe.Sizeof(Header{})]byte {
    var wire [unsafe.Sizeof(Header{})]byte
    binary.BigEndian.PutUint32(wire[:4], header.Version)
    if header.Flags != 0 {
        binary.BigEndian.PutUint32(wire[4:], header.Flags)
    }
    return wire
}

func reset() {}
''')
    assert [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
            for f in result] == [('encodeHeader', 8, 15, 2), ('reset', 17, 17, 1)]


def test_bodyless_array_result_keeps_the_following_function():
    for terminator in ('', ';'):
        result = get_go_function_list('''package wire

import "unsafe"

type Header struct{ Version, Flags uint32 }

func encodeHeader(header Header) [unsafe.Sizeof(Header{})]byte''' + terminator + '''

func reset(flags uint32) uint32 {
    if flags == 0 {
        return 1
    }
    return 0
}
''')
        assert [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
                for f in result] == [('encodeHeader', 7, 7, 1), ('reset', 9, 14, 2)]


def test_bodyless_array_result_at_end_of_file():
    source = '''package wire

import "unsafe"

type Header struct{ Version, Flags uint32 }

func reset(flags uint32) uint32 {
    if flags == 0 {
        return 1
    }
    return 0
}

func encodeHeader(header Header) [unsafe.Sizeof(Header{})]byte'''
    for suffix in ('', '\n', ' // implemented in assembly\n', ';'):
        result = get_go_function_list(source + suffix)
        assert [(f.name, f.start_line, f.end_line, f.cyclomatic_complexity)
                for f in result] == [('reset', 7, 12, 2), ('encodeHeader', 14, 14, 1)]
