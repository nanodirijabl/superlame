-module(stdm_definition_term).

-include("definitions.hrl").

-export([expand_list/3]).

-behaviour(stdm_definition).
-export([new/1]).
-export([expand/4]).

-export_type([t/0]).

-type t() :: term().

new(Term) ->
    Term.

expand(_ID, Term, Container, Visited) ->
    expand_term(Term, Container, Visited).

expand_term(Term, Container, Visited) ->
    hd(expand_list([Term], Container, Visited)).

%%

expand_list([], _Container, _Visited) ->
    [];
expand_list(Args, Container, Visited) when is_list(Args) ->
    F = fun
        (?DEFINITION_REF(ID)) ->
            lists:member(ID, Visited) andalso
                erlang:throw({circular_reference_found, ID, Visited}),
            stdm_container:get(ID, Container, Visited);
        (V0) when is_map(V0) ->
            {Keys, V1} = lists:unzip(maps:to_list(V0)),
            V2 = expand_list(V1, Container, Visited),
            maps:from_list(lists:zip(Keys, V2));
        (V) when is_tuple(V) ->
            list_to_tuple(expand_list(tuple_to_list(V), Container, Visited));
        (V) when is_list(V) ->
            expand_list(V, Container, Visited);
        (V) ->
            V
    end,
    lists:map(F, Args).
