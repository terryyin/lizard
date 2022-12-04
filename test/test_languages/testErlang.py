import unittest
from lizard import analyze_file, FileAnalyzer, get_extensions


def get_erlang_fileinfo(source_code):
    return analyze_file.analyze_source_code("a.erl", source_code)


def get_erlang_function_list(source_code):
    return get_erlang_fileinfo(source_code).function_list


class TestErlang(unittest.TestCase):

    def test_main(self):
        result = get_erlang_function_list('''
        tail_recursive_fib(N) ->
            tail_recursive_fib(N, 0, 1, []).
        lookup(_K, _Tree = ?EMPTY_NODE) ->
            {none, 'undefined'};
        lookup(K, _Tree = {node, {NodeK, V, Left, Right}}) ->
            if K == NodeK -> {ok, V}
            ; K <  NodeK -> lookup(K, Left)
            ; K >  NodeK -> lookup(K, Right)
        end.
        ''')
        self.assertEqual(3, len(result))
        self.assertEqual('tail_recursive_fib', result[0].name)
        self.assertEqual('lookup', result[1].name)
        self.assertEqual('lookup', result[2].name)

    def test_return(self):
        result = get_erlang_function_list('''
        replace(Whole,Old,New) ->
            OldLen = length(Old),
            ReplaceInit = fun (Next, NewWhole) ->
                      case lists:prefix(Old, [Next|NewWhole]) of
                          true ->
                              {_,Rest} = lists:split(OldLen-1, NewWhole),
                              New ++ Rest;
                          false -> [Next|NewWhole]
                      end
                  end,
        lists:foldr(ReplaceInit, [], Whole).
        ''')
        self.assertEqual(2, len(result))
        self.assertEqual('fun', result[0].name)
        self.assertEqual('replace', result[1].name)

    def test_if(self):
        result = get_erlang_function_list('''
        insert([{K, V}|Rest], Tree) ->
            insert(Rest, insert(K, V, Tree)).
        ''')
        self.assertEqual(1, len(result))
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_generic(self):
        result = get_erlang_function_list('''
        %% @doc Insert a new Key into the Tree.
        %%      If the Key already exists, it will be replaced.
        %%
        %% @spec insert(integer(), term(), tree()) -> tree().
        insert(K, V, _Tree = ?EMPTY_NODE) ->
            {node, {K, V, init(), init()}};
        insert(K, V, _Tree = {node, {NodeK, NodeV, Left, Right}}) ->
            if K == NodeK -> % replace
                {node, {K, V, Left, Right}}
            ; K  < NodeK ->
                {node, {NodeK, NodeV, insert(K, V, Left), Right}}
            ; K  > NodeK ->
                {node, {NodeK, NodeV, Left, insert(K, V, Right)}}
            end.

        %% @private
        insert([], Tree) -> Tree;
        insert([{K, V}|Rest], Tree) ->
            insert(Rest, insert(K, V, Tree)).
        ''')
        self.assertEqual(4, len(result))
        self.assertEqual('insert', result[0].name)
        self.assertEqual(10, result[1].cyclomatic_complexity)

    def test_generic_with_where(self):
        result = get_erlang_function_list('''
        fn some_function<T, U>(t: T, u: U) -> i32
            where T: Display + Clone,
                  U: Clone + Debug {
                  }
        ''')
        self.assertEqual(1, len(result))
        self.assertEqual(2, result[0].cyclomatic_complexity)

    def test_nested_functions(self):
        result = get_erlang_function_list('''
        fn main() {
            let x = 4;

            fn equal_to_x(z: i32) -> bool { z == x }

            let y = 4;

            assert!(equal_to_x(y));
        }
        ''')
        self.assertEqual(2, len(result))
