-module(money).

-export(['RUB'/1]).

-export_type([t/0]).

-type t() :: {Amount :: non_neg_integer(), CurrencyCode :: binary()}.

'RUB'(Amount) ->
    {Amount, ~"RUB"}.
