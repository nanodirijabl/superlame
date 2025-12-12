-module(fingy_provider).

-export([bind_transaction/4]).
-export([process_transaction/2]).

-callback bind_transaction(
    payment_pb:'PayerID'(), payment_pb:'ProviderID'(), payment_pb:'Money'()
) ->
    {ok, payment_pb:'SessionTransaction'()} | {error, Reason :: term()}.

-callback process_transaction(payment_pb:'SessionTransaction'()) ->
    ok | {error, Reason :: term()}.

bind_transaction(Mod, PayerID, ProviderID, Amount) ->
    Mod:bind_transaction(PayerID, ProviderID, Amount).

process_transaction(Mod, Transaction) ->
    Mod:process_transaction(Transaction).
