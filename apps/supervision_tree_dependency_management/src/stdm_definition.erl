-module(stdm_definition).

-include("definitions.hrl").

-export_type([t/0, id/0, ref/0]).
-export([new/2]).
-export([ref/1]).
-export([expand/4]).

-type ref() :: ?DEFINITION_REF(id()).
-type id() :: term().
-type t() :: #stdm_definition{}.

-callback new(raw_definition()) -> raw_definition().
-callback expand(
    id(),
    raw_definition(),
    stdm_container:t(),
    [stdm_definition:id()]
) ->
    stdm_container:entry() | no_return().

-spec new(module(), raw_definition()) -> t().
new(Mod, Value) ->
    ?DEFINITION(Mod, Mod:new(Value)).

-spec ref(id()) -> ref().
ref(ID) ->
    ?DEFINITION_REF(ID).

-spec expand(id(), t(), stdm_container:t(), [id()]) ->
    stdm_container:entry() | no_return().
expand(ID, ?DEFINITION(Mod, Value), Container, Visited) ->
    try
        Mod:expand(ID, Value, Container, Visited)
    catch
        error:function_clause ->
            erlang:throw({not_supported, Mod, Value})
    end.
