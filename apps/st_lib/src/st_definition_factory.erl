-module(st_definition_factory).

-behaviour(st_definition).
-export([new/1]).
-export([expand/4]).

-export_type([t/0]).

-type t() ::
    fun(() -> any())
    | fun((st_container:t()) -> any())
    | {function(), [term()]}.

new({Fun, Args} = Spec) when
    is_list(Args) andalso is_function(Fun, length(Args))
->
    Spec;
new(Fun) when is_function(Fun, 0) orelse is_function(Fun, 1) ->
    Fun.

expand(_ID, Fun, Container, Visited) when is_function(Fun, 0) ->
    expand_value(Fun(), Container, Visited);
expand(_ID, Fun, Container, Visited) when is_function(Fun, 1) ->
    expand_value(Fun(Container), Container, Visited);
expand(_ID, {Fun, Args}, Container, Visited) when
    is_list(Args) andalso is_function(Fun, length(Args))
->
    expand_value(erlang:apply(Fun, Args), Container, Visited).

%%

expand_value(Term, Container, Visited) ->
    hd(st_definition_term:expand_list([Term], Container, Visited)).
