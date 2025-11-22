-module(goofy_commander).

-callback command(goofy_entity:state(), goofy_context:t()) ->
    {ok, undefined | goofy_entity:command()} | {error, Reason :: term()}.
