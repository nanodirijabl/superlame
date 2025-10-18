-module(st_definition_supervisor).

-include("definitions.hrl").

-behaviour(supervisor).
-export([init/1]).

-behaviour(st_definition).
-export([new/1]).
-export([expand/4]).

-export_type([t/0]).

-type supervisor_opts() :: {supervisor:sup_flags(), [supervisor:child_spec()]}.

-type t() :: #{
    id => st_definition_child_spec:child_id(),
    flags := supervisor:sup_flags(),
    children := [
        supervisor:child_spec()
        | st_definition:ref()
        | st_definition:t()
    ]
}.

-spec init(supervisor_opts()) -> {ok, supervisor_opts()}.
init({Flags, Specs}) ->
    {ok, {Flags, Specs}}.

-spec new(t()) -> t().
new(#{flags := _Flags, children := Children} = Spec) when is_list(Children) ->
    Spec.

expand(ID, #{flags := Flags, children := Children} = Spec, Container, Visited) ->
    ChildSpec = maps:merge(
        #{
            type => supervisor,
            start =>
                {supervisor, start_link, [
                    ?MODULE,
                    {Flags, expand_children(Children, Container, Visited)}
                ]}
        },
        maps:with([id], Spec)
    ),
    st_definition_child_spec:expand(ID, ChildSpec, Container, Visited).

%%

expand_children(Children, Container, Visited) ->
    F = fun
        (?DEFINITION_REF(ID)) ->
            lists:member(ID, Visited) andalso
                erlang:throw({circular_reference_found, ID, Visited}),
            st_container:get(ID, Container, Visited);
        (?DEFINITION(Mod, _) = Definition) when
            Mod =:= ?MODULE orelse Mod =:= st_definition_child_spec
        ->
            st_definition:expand(undefined, Definition, Container, Visited);
        (?DEFINITION(_, _) = Definition) ->
            erlang:throw({bad_spec, Definition});
        (PlainChildSpec) ->
            PlainChildSpec
    end,
    lists:map(F, Children).
