-module(stdm_tests_SUITE).

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-type config() :: [{atom(), term()}].
-type group_name() :: atom().
-type testdm_case_name() :: atom().

%%

-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).

%%

-export([start_tree/1]).
-export([start_tree_inlined/1]).

%%

-spec all() -> [testdm_case_name() | {group, group_name()}].
all() ->
    [
        {group, main}
    ].

-spec groups() -> [{group_name(), list(), [testdm_case_name()]}].
groups() ->
    [
        {main, [parallel], [
            start_tree,
            start_tree_inlined
        ]}
    ].

-spec init_per_suite(config()) -> config().
init_per_suite(C) ->
    C.

-spec end_per_suite(config()) -> ok.
end_per_suite(_C) ->
    ok.

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(_Name, C) ->
    C.

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(_Name, _C) ->
    ok.

%%

-define(one_for_all(Children),
    stdm:def(supervisor, #{
        flags => #{strategy => one_for_all}, children => Children
    })
).
-define(worker, stdm:def(gen_server, start_link, [?MODULE, [], []])).

-spec start_tree(config()) -> ok.
start_tree(_C) ->
    DI = stdm:new(#{
        ~"sup1" => ?one_for_all([
            stdm:ref(~"worker"), stdm:ref(~"sup2")
        ]),
        ~"sup2" => ?one_for_all([stdm:ref(~"worker")]),
        ~"worker" => ?worker
    }),
    {ok, Pid} = stdm:start_link(stdm:get(~"sup1", DI)),
    ok = proc_lib:stop(Pid),
    ok.

-spec start_tree_inlined(config()) -> ok.
start_tree_inlined(_C) ->
    Definition = ?one_for_all([
        ?worker,
        ?one_for_all([
            ?worker,
            ?one_for_all([
                ?worker,
                ?worker,
                ?one_for_all([
                    ?worker
                ]),
                ?worker
            ])
        ])
    ]),
    {ok, Pid} = stdm:start_link(Definition),
    ok = proc_lib:stop(Pid),
    ok.

%%

-type state() :: [].

-spec init([]) -> {ok, state()}.
init([]) ->
    {ok, []}.

-spec handle_call(_Call, _From, state()) -> {noreply, state()}.
handle_call(_Call, _From, State) ->
    {noreply, State}.

-spec handle_cast(_Cast, state()) -> {noreply, state()}.
handle_cast(_Cast, State) ->
    {noreply, State}.
