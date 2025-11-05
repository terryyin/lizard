import unittest
from lizard import analyze_file


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

    def test_empty(self):
        result = get_erlang_function_list('''

                ''')
        self.assertEqual(0, len(result))

    def test_nested(self):
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

    def test_func_usage(self):
        result = get_erlang_function_list('''
        insert([{K, V}|Rest], Tree) ->
            insert(Rest, insert(K, V, Tree)).
        ''')
        self.assertEqual(1, len(result))
        self.assertEqual(1, result[0].cyclomatic_complexity)

    def test_comments(self):
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
        self.assertEqual(2, result[1].cyclomatic_complexity)

    def test_many_func_usages(self):
        result = get_erlang_function_list('''
        sim_trans() ->
            sim_trans([]).

        sim_trans(ExtraOptions) ->
            Options = [{dict_insert, {filter, mgr_actors}, fun mgr_actors/1}],
            {ok, Viewer} = et_viewer:start_link(Options ++ ExtraOptions),
            Collector = et_viewer:get_collector_pid(Viewer),
            et_collector:report_event(Collector, 60, my_shell, mnesia_tm, start_outer, 
                                      "Start outer transaction"),
            et_collector:report_event(Collector, 40, mnesia_tm, my_shell, new_tid, 
                                      "New transaction id is 4711"),
            et_collector:report_event(Collector, 20, my_shell, mnesia_locker, try_write_lock, 
                                      "Acquire write lock for {my_tab, key}"),
            et_collector:report_event(Collector, 10, mnesia_locker, my_shell, granted,
                                      "You got the write lock for {my_tab, key}"),
            et_collector:report_event(Collector, 60, my_shell, do_commit,
                                      "Perform  transaction commit"),
            et_collector:report_event(Collector, 40, my_shell, mnesia_locker, release_tid,
                                      "Release all locks for transaction 4711"),
            et_collector:report_event(Collector, 60, my_shell, mnesia_tm, delete_transaction,
                                      "End of outer transaction"),
            et_collector:report_event(Collector, 20, my_shell, end_outer,
                                      "Transaction returned {atomic, ok}"),
            {collector, Collector}.
        ''')
        self.assertEqual(2, len(result))
        self.assertEqual(1, result[1].cyclomatic_complexity)

    def test_advanced(self):
        result = get_erlang_function_list('''
        module_as_actor(E) when is_record(E, event) ->
        case lists:key_search(mfa, 1, E#event.contents) of
            {value, {mfa, {M, F, _A}}} ->
                case lists:key_search(pam_result, 1, E#event.contents) of
                    {value, {pam_result, {M2, _F2, _A2}}} ->
                        {true, E#event{label = F, from = M2, to = M}};
                    _ ->
                        {true, E#event{label = F, from = M, to = M}}
                end;
            _ ->
                false
        end.
        ''')
        self.assertEqual(1, len(result))
        self.assertEqual(3, result[0].cyclomatic_complexity)


