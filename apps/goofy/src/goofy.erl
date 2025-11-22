-module(goofy).

-export_type([context/0]).
-export_type([command/0]).
-export_type([state/0]).
-export_type([opts/0]).
-export_type([event/0]).

-type context() :: #{
    flow_id := binary(),
    run_id := binary(),
    task_id := binary(),
    idempotency_key := binary(),
    causation_key := binary(),
    correlation_key := binary(),
    started_at := pos_integer()
}.
-opaque command() :: term().
-opaque state() :: term().
-opaque opts() :: term().
-opaque event() :: term().

-callback handle_command(command(), state(), opts(), context()) ->
    {ok, [event()]} | {error, Reason :: term()}.

-callback apply_event(event(), state()) ->
    {ok, state()} | {error, Reason :: term()}.
