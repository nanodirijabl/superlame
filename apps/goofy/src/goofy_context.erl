-module(goofy_context).

-export([new/3]).
-export([with_idempotency/2]).
-export([with_causation/2]).
-export([with_correlation/2]).

-export_type([t/0]).

-type t() :: #{
    flow_id := binary(),
    run_id := binary(),
    task_id := binary(),
    idempotency_key := undefined | binary(),
    causation_key := undefined | binary(),
    correlation_key := undefined | binary(),
    started_at := binary()
}.

-spec new(binary(), binary(), binary()) -> t().
new(FlowID, RunID, TaskID) ->
    #{
        flow_id => FlowID,
        run_id => RunID,
        task_id => TaskID,
        idempotency_key => undefined,
        causation_key => undefined,
        correlation_key => undefined,
        started_at => goofy_util:to_rfc3339(goofy_util:microseconds_now())
    }.

-spec with_idempotency(binary(), t()) -> t().
with_idempotency(Key, Ctx) ->
    Ctx#{idempotency_key := Key}.

-spec with_causation(binary(), t()) -> t().
with_causation(NewTaskID, #{task_id := CausedByTaskID} = Ctx) ->
    Ctx#{
        task_id := NewTaskID,
        causation_key := CausedByTaskID,
        started_at := goofy_util:to_rfc3339(goofy_util:microseconds_now())
    }.

-spec with_correlation(binary(), t()) -> t().
with_correlation(NewTaskID, #{task_id := RelatedTaskID} = Ctx) ->
    Ctx#{
        task_id := NewTaskID,
        correlation_key := RelatedTaskID,
        started_at := goofy_util:to_rfc3339(goofy_util:microseconds_now())
    }.
