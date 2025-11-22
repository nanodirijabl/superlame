-module(money).

-export_type([t/0]).

-type t() :: {Amount :: non_neg_integer(), CurrencyCode :: binary()}.
