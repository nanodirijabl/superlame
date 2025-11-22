-module(fingy_provider).

-export([bind_transaction/4]).
-export([process_transaction/2]).

-export_type([id/0]).
-export_type([transaction/0]).

-type id() :: binary().
-type transaction() :: {TransactionID :: binary(), Details :: term()}.

-callback bind_transaction(fingy_payment:payer(), id(), money:t()) ->
    {ok, transaction()} | {error, Reason :: term()}.

-callback process_transaction(transaction()) -> ok | {error, Reason :: term()}.

bind_transaction(Mod, Payer, ProviderID, Amount) ->
    Mod:bind_transaction(Payer, ProviderID, Amount).

process_transaction(Mod, Transaction) ->
    Mod:process_transaction(Transaction).
