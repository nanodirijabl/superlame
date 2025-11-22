-module(fingy_accounter).

-export([get_payer_account/2]).
-export([get_provider_account/2]).

-export_type([account/0]).

-type account() :: binary().

-callback get_payer_account(fingy_payment:payer()) ->
    {ok, account()} | {error, not_found}.

-callback get_provider_account(fingy_provider:id()) ->
    {ok, account()} | {error, not_found}.

get_payer_account(Mod, Payer) ->
    Mod:get_payer_account(Payer).

get_provider_account(Mod, Merchant) ->
    Mod:get_provider_account(Merchant).
