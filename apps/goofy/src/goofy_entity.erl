-module(goofy_entity).

-export_type([command/0]).
-export_type([state/0]).
-export_type([event/0]).

-opaque command() :: term().
-opaque state() :: term().
-opaque event() :: term().

-callback handle_command(command(), state(), goofy:opts(), goofy_context:t()) ->
    {ok, [event()]} | {error, Reason :: term()}.
