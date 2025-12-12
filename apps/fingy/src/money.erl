-module(money).

-include_lib("omni/include/payment_pb.hrl").

-export(['RUB'/1]).

'RUB'(Amount) ->
    #'Money'{amount = Amount, currency = ?FUNCTION_NAME}.
