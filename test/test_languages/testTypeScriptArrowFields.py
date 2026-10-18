import unittest

from .testTypeScript import get_ts_function_list


class Test_TypeScript_arrow_field_members(unittest.TestCase):
    def test_class_arrow_field(self):
        """Class arrow field should use the field name, not (anonymous)"""
        code = '''
        class Btn {
            handleClick = () => { console.log("click"); };
            render() { return null; }
        }
        '''
        functions = get_ts_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("handleClick", names)
        self.assertIn("render", names)


class Test_ts_async_unparenthesized_arrow(unittest.TestCase):
    """async field = async x => {} must not corrupt parser state."""

    def test_async_unparenthesized_arrow_in_class(self):
        """field = async x => {} should not break subsequent methods"""
        code = '''
        class Foo {
            before() { return 1; }
            handler = async x => { return x; };
            after() { return 2; }
        }
        '''
        functions = get_ts_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("before", names)
        self.assertIn("after", names)

    def test_large_class_methods_after_async_arrow(self):
        """LWC pattern: methods after async unparenthesized arrow must all be detected"""
        code = '''
        class ItemConfiguration {
            extractRules() { return {}; }
            runExternalDelete = async recordIdsInserted => {
                if (typeof this.externalDelete === "function" && recordIdsInserted.length > 0) {
                    await this.externalDelete(recordIdsInserted);
                }
                return true;
            };
            runExternalReset(record) {
                if (typeof this.externalReset === "function") { this.externalReset(record); }
            }
            get tableClass() { return this.loading ? "slds-hide" : "slds-show"; }
            deleteRecord(isDeleted, itemIndex) {
                try { this.records.splice(itemIndex, 1); } catch (err) { console.error(err); }
            }
            bulkDeleteRecords(recordIds) {
                recordIds.forEach(id => { this.deleteRecord(true, id); });
            }
            copyRecord(index) { return JSON.parse(JSON.stringify(this.records[index])); }
        }
        '''
        functions = get_ts_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("extractRules", names)
        self.assertIn("runExternalReset", names)
        self.assertIn("get tableClass", names)
        self.assertIn("deleteRecord", names)
        self.assertIn("bulkDeleteRecords", names)
        self.assertIn("copyRecord", names)
        self.assertNotIn("if", names)



class Test_ts_class_field_arrow_naming(unittest.TestCase):
    """Class field arrows (field = () => {}) should be named by the field."""

    def test_basic_class_field_arrow(self):
        """handleClick = () => {} should be named handleClick"""
        code = '''
        class Btn {
            handleClick = () => { console.log("click"); };
        }
        '''
        functions = get_ts_function_list(code)
        self.assertEqual(["handleClick"], [f.name for f in functions])

    def test_multiple_class_field_arrows(self):
        """Multiple field arrows should each get their field name"""
        code = '''
        class Form {
            handleSubmit = () => { this.submit(); };
            handleReset = () => { this.reset(); };
            validate = (value: string) => { return value.length > 0; };
        }
        '''
        functions = get_ts_function_list(code)
        self.assertEqual(
            ["handleSubmit", "handleReset", "validate"],
            [f.name for f in functions])

    def test_field_arrows_mixed_with_methods(self):
        """Field arrows and regular methods should all be correctly named"""
        code = '''
        class UserDashboard {
            handleLogin = () => { this.login(); };
            handleLogout = () => { this.logout(); };
            render() { return null; }
            componentDidMount() { this.fetchData(); }
        }
        '''
        functions = get_ts_function_list(code)
        self.assertEqual(
            ["handleLogin", "handleLogout", "render", "componentDidMount"],
            [f.name for f in functions])

    def test_typed_field_arrow(self):
        """Field arrow with type annotation should use field name"""
        code = '''
        class Api {
            fetchData = async (url: string): Promise<Response> => {
                return fetch(url);
            };
        }
        '''
        functions = get_ts_function_list(code)
        self.assertEqual(["fetchData"], [f.name for f in functions])

    def test_field_arrow_after_typed_property(self):
        """Field arrow after a typed property (not arrow) should be named"""
        code = '''
        class Counter {
            count: number;
            increment = () => { this.count++; };
            decrement = () => { this.count--; };
            getCount() { return this.count; }
        }
        '''
        functions = get_ts_function_list(code)
        self.assertEqual(
            ["increment", "decrement", "getCount"],
            [f.name for f in functions])

    def test_static_field_arrow(self):
        """Static field arrows should be detected (name may vary)"""
        code = '''
        class Logger {
            static instance = () => { return new Logger(); };
            log() { console.log("msg"); }
        }
        '''
        functions = get_ts_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("log", names)
        self.assertEqual(2, len(functions))

    def test_inline_callbacks_remain_anonymous(self):
        """Inline callbacks (.map, .filter) should stay (anonymous)"""
        code = '''
        class Processor {
            process(items: string[]) {
                return items
                    .map(x => x.trim())
                    .filter(x => x.length > 0);
            }
        }
        '''
        functions = get_ts_function_list(code)
        names = [f.name for f in functions]
        self.assertIn("process", names)
        anon_count = sum(1 for n in names if n == "(anonymous)")
        self.assertGreaterEqual(anon_count, 2)
