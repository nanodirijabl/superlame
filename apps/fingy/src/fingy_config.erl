-module(fingy_config).

-export([find_provider_for_merchant/2]).

-callback find_provider_for_merchant(payment_pb:'MerchantID'()) ->
    {ok, payment_pb:'ProviderID'()} | {error, not_found}.

find_provider_for_merchant(Mod, Merchant) ->
    Mod:find_provider_for_merchant(Merchant).
