import unittest

from .testTypeScript import get_ts_function_list


class Test_TypeScript_typed_fields(unittest.TestCase):
    def test_typed_field_followed_by_methods(self):
        code = '''
        class Svc {
            count: number;
            private helper(): number { return 1; }
            exposed(): number { return 2; }
        }
        '''
        functions = get_ts_function_list(code)
        names = [f.name for f in functions]
        self.assertIn('helper', names)
        self.assertIn('exposed', names)

    def test_typed_field_with_initializer_followed_by_methods(self):
        code = '''
        class Svc {
            private state: any = { x: 1 };
            doWork(): void { return; }
        }
        '''
        functions = get_ts_function_list(code)
        names = [f.name for f in functions]
        self.assertIn('doWork', names)


class Test_TypeScript_static_field_class_parsing(unittest.TestCase):
    """Tests that static field = {} does not break subsequent class method detection (Fix 4)."""

    def test_static_defaultprops_then_methods(self):
        """Methods after static defaultProps = {...} should be detected"""
        code = '''
        class Comp {
            static defaultProps = { color: "red", size: 10 };
            static displayName = "Comp";
            render() { return null; }
            handleClick() { return true; }
        }
        '''
        functions = get_ts_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("render", names)
        self.assertIn("handleClick", names)
        # static field assignments should NOT be functions
        self.assertNotIn("defaultProps", names)
        self.assertNotIn("displayName", names)



class Test_ts_dot_field_assignment(unittest.TestCase):
    """Tests that field = OBJ.PROP does not break subsequent method detection."""

    def test_dot_field_assignment(self):
        """field = A.B; should not swallow the next method"""
        code = 'class C { x = A.B; m1() {} m2() {} }'
        functions = get_ts_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("m1", names)
        self.assertIn("m2", names)

    def test_dot_field_with_type_annotation(self):
        """Typed field = OBJ.PROP should not swallow methods"""
        code = '''
        class C {
            mode: string = MODE.STANDARD;
            method1(): void { return; }
            method2(): number { return 1; }
        }
        '''
        functions = get_ts_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("method1", names)
        self.assertIn("method2", names)

    def test_multiple_dot_fields_lwc_pattern(self):
        """LWC pattern: multiple OBJ.PROP fields followed by methods"""
        code = '''
        class Widget {
            settings = ITEM_ACTION_MODAL_SETTINGS;
            modalMode = ITEM_ACTIONS.SPLIT;
            status = AVAILABILITY_STATUS.PENDING;
            getSettings() { return this.settings; }
            getMode() { return this.modalMode; }
            render() { return null; }
        }
        '''
        functions = get_ts_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("getSettings", names)
        self.assertIn("getMode", names)
        self.assertIn("render", names)


class Test_TypeScript_member_modifier_after_a_field(unittest.TestCase):
    """A field without ';' ends at the line of the next member's modifier."""

    def test_methods_with_modifiers_after_fields_without_semicolons(self):
        code = (
            "class Store {\n"
            "  count = 0\n"
            "  static create(): Store {\n"
            "    return new Store();\n"
            "  }\n"
            "  limit: number = 10\n"
            "  async load(x: number): Promise<number> {\n"
            "    return x;\n"
            "  }\n"
            "  name = 'a'\n"
            "  get value(): number {\n"
            "    return this.count;\n"
            "  }\n"
            "}\n"
        )
        self.assertEqual(
            [('create', 3, 5), ('load', 7, 9), ('get value', 11, 13)],
            [(f.name, f.start_line, f.end_line)
             for f in get_ts_function_list(code)])


class Test_TypeScript_modifier_named_object_keys(unittest.TestCase):
    """`static`/`async` used as object keys are names, not modifiers; read
    as modifiers they swallowed the object's '}' and hid later callbacks."""

    def assert_functions(self, code, expected):
        self.assertEqual(
            expected,
            [(f.name, f.cyclomatic_complexity, f.start_line, f.end_line)
             for f in get_ts_function_list(code)])

    def test_static_key_then_callback_argument(self):
        self.assert_functions(
            "const a = { static: 1 };\n"
            "const b = g();\n"
            "const d = g(\n"
            "  x,\n"
            "  (arg) => {\n"
            "    if (arg) {\n"
            "    }\n"
            "  },\n"
            ");\n",
            [('(anonymous)', 2, 5, 8)])

    def test_modifier_keys_inside_arrow_body(self):
        for key in ('static', 'async'):
            with self.subTest(key=key):
                self.assert_functions(
                    "const s = f(() => {\n"
                    "  return { %s: p[x] };\n"
                    "});\n"
                    "const u = g(`x`, (arg) => {\n"
                    "  if (arg) {}\n"
                    "});\n" % key,
                    [('(anonymous)', 1, 1, 3), ('(anonymous)', 2, 4, 6)])

    def test_shorthand_modifier_keys(self):
        self.assert_functions(
            "const a = { x, async, static };\n"
            "const b = g();\n"
            "function h() { if (x) {} }\n",
            [('h', 2, 3, 3)])
