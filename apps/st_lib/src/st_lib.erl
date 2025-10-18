-module(st_lib).

-include("definitions.hrl").

-export([start_link/1, start_link/2]).

-export([new/0, new/1]).
-export([set/3]).
-export([get/2]).

-export([ref/1]).
-export([factory/1, factory/2]).
-export([value/1]).
-export([def/1, def/2, def/3]).

-type standard_type() ::
    supervisor.

start_link(Definition) ->
    start_link(Definition, new()).

start_link(?DEFINITION(_, _) = Definition, Container) ->
    do_start_link(st_definition:expand(undefined, Definition, Container, []));
start_link(#{start := {_M, _F, _Args}} = Spec, _Container) ->
    do_start_link(Spec).

new() ->
    st_container:new().

new(Definitions) ->
    st_container:new(Definitions).

set(ID, Definition, Container) ->
    st_container:set(ID, Definition, Container).

get(ID, Container) ->
    st_container:get(ID, Container).

-spec factory(st_definition_factory:t()) -> st_definition:t().
factory(Spec) ->
    st_definition:new(st_definition_factory, Spec).

-spec factory(function(), [term()]) -> st_definition:t().
factory(Fun, Args) when is_list(Args) andalso is_function(Fun, length(Args)) ->
    st_definition:new(st_definition_factory, {Fun, Args}).

-spec value(st_definition_term:t()) -> st_definition:t().
value(Value) ->
    st_definition:new(st_definition_term, Value).

-spec def(st_definition_child_spec:t()) -> st_definition:t().
def(Spec) ->
    st_definition:new(st_definition_child_spec, Spec).

-spec def(standard_type(), st_definition_supervisor:t()) ->
    st_definition:t().
def(supervisor, Spec) ->
    st_definition:new(st_definition_supervisor, Spec).

-spec def(module(), atom(), [term()]) -> st_definition:t().
def(Mod, Fun, Args) when
    is_atom(Mod) andalso is_atom(Fun) andalso is_list(Args)
->
    def(#{start => {Mod, Fun, Args}}).

ref(ID) ->
    st_definition:ref(ID).

do_start_link(#{start := {Module, Function, Args}}) ->
    erlang:apply(Module, Function, Args).

%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-define(sup_flags, #{strategy => one_for_all}).
-define(stp_fixture, def(supervisor, #{flags => ?sup_flags, children => []})).
-define(sup_fixture(Children),
    def(supervisor, #{flags => ?sup_flags, children => Children})
).

-spec def_test_() -> [_].
def_test_() ->
    [
        ?_assertEqual(
            ?DEFINITION(st_definition_child_spec, #{
                id => test, start => {my_mod, my_fun, []}
            }),
            def(#{id => test, start => {my_mod, my_fun, []}})
        ),
        ?_assertEqual(
            ?DEFINITION(st_definition_child_spec, #{
                start => {my_mod, my_fun, []}
            }),
            def(#{start => {my_mod, my_fun, []}})
        ),
        ?_assertEqual(
            ?DEFINITION(st_definition_child_spec, #{
                start => {my_mod, my_fun, []}
            }),
            def(my_mod, my_fun, [])
        ),
        ?_assertEqual(
            ?DEFINITION(st_definition_supervisor, #{
                flags => ?sup_flags, children => []
            }),
            def(supervisor, #{flags => ?sup_flags, children => []})
        )
    ].

-spec set_test_() -> [_].
set_test_() ->
    [
        ?_assertEqual(
            #{~"test" => ?stp_fixture},
            set(~"test", ?stp_fixture, new())
        ),
        ?_assertEqual(
            #{
                ~"test1" => ?stp_fixture,
                ~"test2" => ?stp_fixture
            },
            set(
                ~"test1", ?stp_fixture, new(#{~"test2" => ?stp_fixture})
            )
        )
    ].

-spec get_not_found_test_() -> [_].
get_not_found_test_() ->
    [
        ?_assertThrow(
            {not_found, ~"test", []},
            get(~"test", new())
        ),
        ?_assertThrow(
            {not_found, ~"test", []},
            get(~"test", new(#{~"other" => ?stp_fixture}))
        ),
        ?_assertThrow(
            {not_found, ~"sup2", [~"root"]},
            get(
                ~"root",
                new(#{
                    ~"root" => def(
                        supervisor,
                        #{
                            flags => ?sup_flags,
                            children => [ref(~"sup1"), ref(~"sup2")]
                        }
                    ),
                    ~"sup1" => def(supervisor, #{
                        flags => ?sup_flags, children => []
                    })
                })
            )
        )
    ].

-spec get_circular_reference_found_test_() -> [_].
get_circular_reference_found_test_() ->
    WCircularReference = new(#{
        ~"A" => ?sup_fixture([ref(~"B")]),
        ~"B" => ?sup_fixture([ref(~"C")]),
        ~"C" => ?sup_fixture([ref(~"A")])
    }),
    [
        ?_assertThrow(
            {circular_reference_found, ~"A", [~"C", ~"B", ~"A"]},
            get(~"A", WCircularReference)
        ),
        ?_assertThrow(
            {circular_reference_found, ~"B", [~"A", ~"C", ~"B"]},
            get(~"B", WCircularReference)
        )
    ].

-spec get_test_() -> [_].
get_test_() ->
    Container = new(#{
        ~"root" => ?sup_fixture([ref(~"sup1"), ref(~"sup2")]),
        ~"sup1" => ?sup_fixture([]),
        ~"sup2" => ?sup_fixture([ref(~"worker1"), ref(~"sup3")]),
        ~"sup3" => ?sup_fixture([]),
        ~"worker1" => def(#{start => {my_mod, my_fun, []}})
    }),
    [
        ?_assertEqual(
            #{
                id => ~"root",
                type => supervisor,
                start =>
                    {supervisor, start_link, [
                        st_definition_supervisor,
                        {?sup_flags, [
                            #{
                                id => ~"sup1",
                                type => supervisor,
                                start =>
                                    {supervisor, start_link, [
                                        st_definition_supervisor,
                                        {?sup_flags, []}
                                    ]}
                            },
                            #{
                                id => ~"sup2",
                                type => supervisor,
                                start =>
                                    {supervisor, start_link, [
                                        st_definition_supervisor,
                                        {?sup_flags, [
                                            #{
                                                id => ~"worker1",
                                                start => {my_mod, my_fun, []}
                                            },
                                            #{
                                                id => ~"sup3",
                                                type => supervisor,
                                                start =>
                                                    {supervisor, start_link, [
                                                        st_definition_supervisor,
                                                        {?sup_flags, []}
                                                    ]}
                                            }
                                        ]}
                                    ]}
                            }
                        ]}
                    ]}
            },
            get(~"root", Container)
        )
    ].

-spec get_with_args_expansion_test_() -> [_].
get_with_args_expansion_test_() ->
    Container = new(#{
        root => ?sup_fixture([ref({worker, 1})]),
        {worker, 1} => def(#{
            start =>
                {my_mod, my_fun, [
                    ref({nested, 1}),
                    [
                        foo,
                        [ref({nested, 1})],
                        bar,
                        {reference, ref({nested, 1}), #{
                            foo => bar,
                            baz => ref({nested, 1})
                        }}
                    ]
                ]}
        }),
        {nested, 1} => ?stp_fixture
    }),
    Nested = get({nested, 1}, Container),
    [
        ?_assertMatch(
            #{
                start :=
                    {supervisor, start_link, [
                        _Sup,
                        {_SupFlags, [
                            #{
                                start :=
                                    {my_mod, my_fun, [
                                        Nested,
                                        [
                                            foo,
                                            [Nested],
                                            bar,
                                            {reference, Nested, #{
                                                foo := bar, baz := Nested
                                            }}
                                        ]
                                    ]}
                            }
                        ]}
                    ]}
            },
            get(root, Container)
        )
    ].

-spec values_expansion_test_() -> [_].
values_expansion_test_() ->
    Container = new(#{
        foo => value(bar),
        {factory, baz} => factory(fun() -> baz end),
        {factory, foo} => factory(fun(C) -> {ok, get(foo, C)} end),
        {factory, sum} => factory(fun(A, B) -> A + B end, [40, 2]),
        {factory, func_arity_0} => factory(fun func_arity_0/0, []),
        {factory, func_arity_1} => factory(fun func_arity_1/1, [foo])
    }),
    [
        ?_assertEqual(bar, get(foo, Container)),
        ?_assertEqual(baz, get({factory, baz}, Container)),
        ?_assertEqual({ok, bar}, get({factory, foo}, Container)),
        ?_assertEqual(42, get({factory, sum}, Container)),
        ?_assertEqual(ok, get({factory, func_arity_0}, Container)),
        ?_assertEqual({ok, foo}, get({factory, func_arity_1}, Container))
    ].

func_arity_0() ->
    ok.

func_arity_1(V) ->
    {ok, V}.

-endif.
