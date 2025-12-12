-module(goofy_projector).

-callback apply_event(goofy_entity:event(), goofy_entity:state()) ->
    {ok, goofy_entity:state()} | {error, Reason :: term()}.
