-module(shareware_entrypoint).

-export([start_link/1, start_link/2]).

start_link(Definition) ->
    start_link(Definition, shareware:new()).

start_link({'$definition', _} = Definition, Container) ->
    do_start_link(shareware:expand(Definition, Container));
start_link({'$child_spec', Spec}, _Container) ->
    do_start_link(Spec).

%%

do_start_link(#{start := {Module, Function, Args}}) ->
    erlang:apply(Module, Function, Args).
