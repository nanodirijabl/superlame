-module(fingy_config).

-export([find_provider_for_merchant/2]).

-callback find_provider_for_merchant(fingy_payment:merchant()) ->
    {ok, fingy_provider:id()} | {error, not_found}.

find_provider_for_merchant(Mod, Merchant) ->
    Mod:find_provider_for_merchant(Merchant).
