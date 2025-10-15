-module(shareware_entrypoint).

-include("definitions.hrl").

-export([start_link/1, start_link/2]).

start_link(Definition) ->
    start_link(Definition, shareware:new()).

start_link(?DEFINITION(_, _) = Definition, Container) ->
    do_start_link(
        shareware_definition:expand(undefined, Definition, Container, [])
    );
start_link(#{start := {_M, _F, _Args}} = Spec, _Container) ->
    do_start_link(Spec).

%%

do_start_link(#{start := {Module, Function, Args}}) ->
    erlang:apply(Module, Function, Args).
