-module(shareware_container).

-export_type([t/0, entry/0]).
-export([new/0, new/1]).
-export([set/3]).
-export([get/2, get/3]).

-type t() :: #{shareware_definition:id() => shareware_definition:t()}.
-type entry() :: term().

-spec new() -> t().
new() ->
    #{}.

-spec new(#{shareware_definition:id() => shareware_definition:t()}) -> t().
new(Entries) ->
    Entries.

-spec set(shareware_definition:id(), shareware_definition:t(), t()) -> t().
set(ID, Definition, Container) ->
    %% TODO Assert id and entry are valid
    maps:put(ID, Definition, Container).

-spec get(shareware_definition:id(), t()) -> entry().
get(ID, Container) ->
    get(ID, Container, []).

-spec get(shareware_definition:id(), t(), [shareware_definition:id()]) ->
    entry() | no_return().
%% Private function.
get(ID, Container, Visited) ->
    Definition = find(ID, Container, Visited),
    shareware_definition:expand(ID, Definition, Container, [ID | Visited]).

%%

find(ID, Container, Visited) ->
    case maps:get(ID, Container, undefined) of
        undefined -> erlang:throw({not_found, ID, Visited});
        Definition -> Definition
    end.
