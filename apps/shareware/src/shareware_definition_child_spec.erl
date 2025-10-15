-module(shareware_definition_child_spec).

-behaviour(shareware_definition).
-export([new/1]).
-export([expand/4]).

-export_type([t/0]).
-export_type([child_id/0]).

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
-type t() :: #{
    id => child_id(),
    start := mfargs(),
    restart => restart(),
    significant => significant(),
    shutdown => shutdown(),
    type => worker(),
    modules => modules()
}.

-spec new(t()) -> t().
new(#{start := _MFArgs} = Spec) ->
    Spec.

expand(ID, Spec, Container, Visited) ->
    Spec#{
        id => child_spec_id(ID, Spec),
        start := expand_start_args(Spec, Container, Visited)
    }.

child_spec_id(_EntryID, #{id := ID}) ->
    ID;
child_spec_id(undefined, _Spec) ->
    generate_entry_id();
child_spec_id(EntryID, _Spec) ->
    EntryID.

%%

generate_entry_id() ->
    {entry, make_ref()}.

expand_start_args(
    #{start := {Module, Function, undefined}}, _Container, _Visited
) ->
    {Module, Function, undefined};
expand_start_args(#{start := {Module, Function, Args}}, Container, Visited) when
    is_list(Args)
->
    {Module, Function,
        shareware_definition_term:expand_list(Args, Container, Visited)}.
