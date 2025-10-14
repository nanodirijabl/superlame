-module(shareware).

-export([ref/1]).
-export([expand/1, expand/2]).

-export([def/1, def/2, def/3]).

-export([new/0, new/1]).
-export([set/3]).
-export([get/2]).

%%

-behaviour(supervisor).
-export([init/1]).

-type sup_opts() :: {supervisor:sup_flags(), [supervisor:child_spec()]}.

%%

-type service_id() :: term().
-type service_reference() :: {ref, service_id()}.
-type stp_type() :: supervisor.
-type stp_args() :: term().
-type stp_spec() :: #{
    type := stp_type(),
    args := stp_args(),
    id => term()
}.

-type child_id() :: term().
-type mfargs() ::
    {Module :: module(), Function :: atom(), Args :: [term()] | undefined}.
-type modules() :: [module()] | 'dynamic'.
-type restart() :: 'permanent' | 'transient' | 'temporary'.
-type significant() :: boolean().
-type shutdown() :: 'brutal_kill' | timeout().
-type worker() :: 'worker' | 'supervisor'.

%% NOTE Child specs like in supervisor but id is optional.
%% See `supervisor:child_spec/0'.
-type child_spec() :: #{
    id => child_id(),
    start := mfargs(),
    restart => restart(),
    significant => significant(),
    shutdown => shutdown(),
    type => worker(),
    modules => modules()
}.

-type service_definition() ::
    {child_spec, child_spec()}
    | {definition, stp_spec()}.

-type service_container() :: #{service_id() => service_definition()}.
-type service() :: supervisor:child_spec().

%%

-define(SUP, ?MODULE).

-spec init(sup_opts()) -> {ok, sup_opts()}.
init({Flags, Specs}) ->
    {ok, {Flags, Specs}}.

%%

-spec def(child_spec()) -> service_definition().
def(Spec = #{start := _MFArgs}) ->
    {child_spec, Spec}.

-spec def(stp_type(), stp_args()) -> service_definition().
def(supervisor, Args) ->
    {definition, #{type => supervisor, args => Args}}.

-spec def(module(), atom(), [term()]) -> service_definition().
def(Mod, Fun, Args) when
    is_atom(Mod) andalso is_atom(Fun) andalso is_list(Args)
->
    def(#{start => {Mod, Fun, Args}}).

-spec ref(service_id()) -> service_reference().
ref(ID) ->
    {ref, ID}.

-spec new() -> service_container().
new() ->
    #{}.

-spec new(#{service_id() => service_definition()}) -> service_container().
new(Definitions) ->
    Definitions.

-spec set(service_id(), service_definition(), service_container()) ->
    service_container().
set(ID, Definition, Container) ->
    %% TODO Assert id and definition are valid
    maps:put(ID, Definition, Container).

-spec get(service_id(), service_container()) -> service().
get(ID, Container) ->
    get(ID, Container, []).

-spec expand(service_definition()) -> service().
expand(Definition) ->
    expand(undefined, Definition, new(), []).

-spec expand(service_definition(), service_container()) -> service().
expand(Definition, Container) ->
    expand(undefined, Definition, Container, []).

%%

get(ID, Container, Visited) ->
    Definition = find(ID, Container, Visited),
    expand(ID, Definition, Container, Visited).

find(ID, Container, Visited) ->
    case maps:get(ID, Container, undefined) of
        undefined ->
            erlang:throw({service_not_found, lists:reverse([ID | Visited])});
        Definition ->
            Definition
    end.

expand(ID, {child_spec, ChildSpec}, Container, Visited) ->
    %% TODO Recursively expand start MFArgs' arguments list
    ChildSpec#{
        id => child_spec_id(ID, ChildSpec),
        start := expand_start_args(ChildSpec, Container, Visited)
    };
expand(
    ID,
    {definition, S = #{type := supervisor, args := {Flags, Children}}},
    Container,
    Visited
) ->
    #{
        id => child_spec_id(ID, S),
        start =>
            {supervisor, start_link, [
                ?SUP,
                {Flags, expand_children(Children, Container, [ID | Visited])}
            ]},
        type => supervisor
    }.

expand_start_args(#{start := {Module, Function, Args}}, Container, Visited) ->
    {Module, Function, recursively_expand_list(Args, Container, Visited)}.

recursively_expand_list([], _Container, _Visited) ->
    [];
recursively_expand_list(Args, Container, Visited) when is_list(Args) ->
    F = fun
        ({ref, ID}) ->
            lists:member(ID, Visited) andalso
                erlang:throw(
                    {circular_reference_found, ID, lists:reverse(Visited)}
                ),
            get(ID, Container, Visited);
        (V0) when is_map(V0) ->
            {Keys, V1} = lists:unzip(maps:to_list(V0)),
            V2 = recursively_expand_list(V1, Container, Visited),
            maps:from_list(lists:zip(Keys, V2));
        (V) when is_tuple(V) ->
            list_to_tuple(
                recursively_expand_list(tuple_to_list(V), Container, Visited)
            );
        (V) when is_list(V) ->
            recursively_expand_list(V, Container, Visited);
        (V) ->
            V
    end,
    lists:map(F, Args).

expand_children(Children, Container, Visited) ->
    F = fun
        ({ref, ID}) ->
            lists:member(ID, Visited) andalso
                erlang:throw(
                    {circular_reference_found, ID, lists:reverse(Visited)}
                ),
            get(ID, Container, Visited);
        (Definition) when
            element(1, Definition) =:= child_spec orelse
                element(1, Definition) =:= definition
        ->
            expand(undefined, Definition, Container, Visited);
        (ChildSpec) ->
            ChildSpec
    end,
    lists:map(F, Children).

child_spec_id(_ServiceID, #{id := ID}) ->
    ID;
child_spec_id(undefined, _STPSpec) ->
    generate_service_id();
child_spec_id(ServiceID, _STPSpec) ->
    ServiceID.

generate_service_id() ->
    {service, make_ref()}.

%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-define(sup_flags, #{strategy => one_for_all}).
-define(stp_fixture, def(supervisor, {?sup_flags, []})).
-define(sup_fixture(Children), def(supervisor, {?sup_flags, Children})).

-spec def_test_() -> [_].
def_test_() ->
    [
        ?_assertEqual(
            {child_spec, #{id => test, start => {my_mod, my_fun, []}}},
            def(#{id => test, start => {my_mod, my_fun, []}})
        ),
        ?_assertEqual(
            {child_spec, #{start => {my_mod, my_fun, []}}},
            def(#{start => {my_mod, my_fun, []}})
        ),
        ?_assertEqual(
            {child_spec, #{start => {my_mod, my_fun, []}}},
            def(my_mod, my_fun, [])
        ),
        ?_assertEqual(
            {definition, #{type => supervisor, args => {?sup_flags, []}}},
            def(supervisor, {?sup_flags, []})
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
            {service_not_found, [~"test"]},
            get(~"test", new())
        ),
        ?_assertThrow(
            {service_not_found, [~"test"]},
            get(~"test", new(#{~"other" => ?stp_fixture}))
        ),
        ?_assertThrow(
            {service_not_found, [~"root", ~"sup2"]},
            get(
                ~"root",
                new(#{
                    ~"root" => def(
                        supervisor,
                        {?sup_flags, [ref(~"sup1"), ref(~"sup2")]}
                    ),
                    ~"sup1" => def(supervisor, {?sup_flags, []})
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
            {circular_reference_found, ~"A", [~"A", ~"B", ~"C"]},
            get(~"A", WCircularReference)
        ),
        ?_assertThrow(
            {circular_reference_found, ~"B", [~"B", ~"C", ~"A"]},
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
                        ?SUP,
                        {?sup_flags, [
                            #{
                                id => ~"sup1",
                                type => supervisor,
                                start =>
                                    {supervisor, start_link, [
                                        ?SUP, {?sup_flags, []}
                                    ]}
                            },
                            #{
                                id => ~"sup2",
                                type => supervisor,
                                start =>
                                    {supervisor, start_link, [
                                        ?SUP,
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
                                                        ?SUP, {?sup_flags, []}
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

-endif.
