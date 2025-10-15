-module(shareware_supervisor).

-behaviour(supervisor).
-export([init/1]).

-type supervisor_opts() :: {supervisor:sup_flags(), [supervisor:child_spec()]}.

-spec init(supervisor_opts()) -> {ok, supervisor_opts()}.
init({Flags, Specs}) ->
    {ok, {Flags, Specs}}.
