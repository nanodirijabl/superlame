-module(fingy_accounter_stupid).

-export([get_payer_account/1]).
-export([get_provider_account/1]).

get_payer_account(Payer) ->
    <<Payer/binary, "_account">>.

get_provider_account(Provider) ->
    <<Provider/binary, "_account">>.
