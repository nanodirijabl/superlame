-module(shareware_entrypoint).

-export([start_link/1, start_link/2]).

start_link(Definition) ->
    start_link(Definition, shareware:new()).

start_link(Definition, Container) ->
    #{start := {supervisor, start_link, [shareware, Args]}} =
        shareware:expand(Definition, Container),
    supervisor:start_link(shareware, Args).
