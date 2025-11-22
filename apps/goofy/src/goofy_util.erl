-module(goofy_util).

-export([unique/0]).
-export([microseconds_now/0]).
-export([to_rfc3339/1]).

unique() ->
    <<I:160/integer>> = crypto:hash(
        sha, term_to_binary({make_ref(), os:timestamp()})
    ),
    format_int_base(I, 61).

format_int_base(I, Base) when
    is_integer(I), is_integer(Base), Base >= 2, Base =< 62
->
    R = list_to_binary(format_int_base(abs(I), Base, [])),
    %% if
    %%     I > 0 -> R;
    %%     I == 0 -> <<$0>>;
    %%     I < 0 -> <<$-, R/binary>>
    %% end;
    if
        I > 0 -> R;
        I == 0 -> <<$0>>
    end;
format_int_base(I, Base) ->
    error(badarg, [I, Base]).

format_int_base(0, _Base, R0) ->
    R0;
format_int_base(I0, Base, R0) ->
    D = I0 rem Base,
    I1 = I0 div Base,
    R1 =
        if
            D >= 36 -> [D - 36 + $a | R0];
            D >= 10 -> [D - 10 + $A | R0];
            true -> [D + $0 | R0]
        end,
    format_int_base(I1, Base, R1).

microseconds_now() ->
    os:system_time(microsecond).

to_rfc3339(SystemTimeMicroseconds) ->
    erlang:list_to_binary(
        calendar:system_time_to_rfc3339(SystemTimeMicroseconds, [
            {unit, microsecond}, {offset, "Z"}
        ])
    ).
