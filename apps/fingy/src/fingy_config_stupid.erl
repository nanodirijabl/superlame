-module(fingy_config_stupid).

-export([find_provider_for_merchant/1]).

find_provider_for_merchant(Merchant) ->
    {ok, <<Merchant/binary, "_provider">>}.
