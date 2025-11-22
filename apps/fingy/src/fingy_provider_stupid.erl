-module(fingy_provider_stupid).

-export([bind_transaction/3]).
-export([process_transaction/1]).

bind_transaction(Payer, ProviderID, {Amount, Currency}) ->
    {ok, {
        <<Payer/binary, "/", ProviderID/binary, "/",
            (integer_to_binary(Amount))/binary, "/", Currency/binary>>,
        ~"stupid transaction's stupid details"
    }}.

process_transaction(_Transaction) ->
    ok.
