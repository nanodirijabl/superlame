-module(fingy_payment).

-include_lib("omni/include/payment_pb.hrl").

-behaviour(goofy_entity).
-export([handle_command/4]).

-behaviour(goofy_projector).
-export([apply_event/2]).

-behaviour(goofy_commander).
-export([command/2]).

-ifdef(TEST).

-behaviour(fingy_provider).
-export([bind_transaction/3]).
-export([process_transaction/1]).

-behaviour(fingy_config).
-export([find_provider_for_merchant/1]).

-behaviour(fingy_accounter).
-export([get_payer_account/1]).
-export([get_provider_account/1]).

-endif.

-type opts() :: #{
    fingy_accounter := module(),
    fingy_config := module(),
    fingy_provider := module()
}.

-spec handle_command(
    tuple(), undefined | #'Payment'{}, opts(), goofy_context:t()
) ->
    {ok, [tuple()]} | {error, term()}.
handle_command(
    #'StartPaymentCommand'{
        payment_id = ID,
        payer = Payer,
        merchant = Merchant,
        amount = Amount
    },
    undefined,
    _Opts,
    _Context
) ->
    {ok, [
        #'PaymentStartedEvent'{
            payment_id = ID,
            payer = Payer,
            merchant = Merchant,
            amount = Amount
        }
    ]};
handle_command(
    #'FindRouteCommand'{},
    #'Payment'{
        status = {pending, #'PendingStatus'{}},
        merchant = Merchant,
        route = undefined
    },
    Opts,
    _Context
) ->
    ConfigMod = maps:get(fingy_config, Opts),
    case fingy_config:find_provider_for_merchant(ConfigMod, Merchant) of
        {ok, ProviderID} ->
            {ok, [#'RouteFoundEvent'{route = #'Route'{provider = ProviderID}}]};
        {error, not_found} ->
            {ok, [#'PaymentFailedEvent'{reason = ~"route-not-found"}]}
    end;
handle_command(
    #'CalculateCashFlowCommand'{},
    #'Payment'{
        status = {pending, #'PendingStatus'{}},
        payer = Payer,
        amount = Amount,
        route = #'Route'{provider = ProviderID}
    },
    Opts,
    _Context
) ->
    AccounterMod = maps:get(fingy_accounter, Opts),
    maybe
        {ok, PayerAccount} ?=
            fingy_accounter:get_payer_account(AccounterMod, Payer),
        {ok, ProviderAccount} ?=
            fingy_accounter:get_provider_account(AccounterMod, ProviderID),
        CashFlow = #'CashFlow'{
            postings = [
                #'CashFlowPosting'{
                    source = PayerAccount,
                    destination = ProviderAccount,
                    amount = Amount
                }
            ]
        },
        {ok, [#'CashFlowCalculatedEvent'{cash_flow = CashFlow}]}
    end;
handle_command(
    #'StartSessionCommand'{},
    #'Payment'{
        id = PaymentID,
        status = {pending, #'PendingStatus'{}},
        sessions = Sessions
    },
    _Opts,
    _Context
) when
    length(Sessions) =:= 0 orelse
        (hd(Sessions))#'Session'.status =/= {pending, #'PendingStatus'{}}
->
    SessionID =
        <<PaymentID/binary, "/",
            (integer_to_binary(length(Sessions) + 1))/binary>>,
    {ok, [#'SessionStartedEvent'{session_id = SessionID}]};
handle_command(
    #'BindSessionCommand'{},
    #'Payment'{
        status = {pending, #'PendingStatus'{}},
        payer = Payer,
        amount = Amount,
        route = #'Route'{provider = ProviderID},
        sessions = [#'Session'{status = {pending, #'PendingStatus'{}}} | _]
    },
    Opts,
    _Context
) ->
    ProviderMod = maps:get(fingy_provider, Opts),
    case
        fingy_provider:bind_transaction(ProviderMod, Payer, ProviderID, Amount)
    of
        {ok, Transaction} ->
            {ok, [#'SessionBoundEvent'{transaction = Transaction}]};
        {error, Reason} ->
            {ok, [
                #'SessionFailedEvent'{
                    reason = iolist_to_binary(io_lib:format("~p", [Reason]))
                }
            ]}
    end;
handle_command(
    #'ProcessSessionCommand'{},
    #'Payment'{
        status = {pending, #'PendingStatus'{}},
        sessions = [
            #'Session'{
                status = {pending, #'PendingStatus'{}},
                transaction = Transaction
            }
            | _
        ]
    },
    Opts,
    _Context
) ->
    ProviderMod = maps:get(fingy_provider, Opts),
    case fingy_provider:process_transaction(ProviderMod, Transaction) of
        ok ->
            {ok, [#'SessionProcessedEvent'{}]};
        {error, Reason} ->
            {ok, [#'SessionFailedEvent'{reason = Reason}]}
    end;
handle_command(
    #'CapturePaymentCommand'{},
    #'Payment'{
        status = {pending, #'PendingStatus'{}},
        sessions = [#'Session'{status = {complete, #'CompleteStatus'{}}} | _]
    },
    _Opts,
    _Context
) ->
    {ok, [#'PaymentCompletedEvent'{}]};
handle_command(Command, Payment, _Opts, _Context) ->
    {error, {unexpected, Command, Payment}}.

apply_event(
    #'PaymentStartedEvent'{
        payment_id = ID,
        payer = Payer,
        merchant = Merchant,
        amount = Amount
    },
    undefined
) ->
    {ok, #'Payment'{
        id = ID,
        status = {pending, #'PendingStatus'{}},
        payer = Payer,
        merchant = Merchant,
        amount = Amount,
        route = undefined,
        cash_flow = undefined,
        sessions = []
    }};
apply_event(#'RouteFoundEvent'{route = Route}, Payment) ->
    {ok, Payment#'Payment'{route = Route}};
apply_event(#'CashFlowCalculatedEvent'{cash_flow = CashFlow}, Payment) ->
    {ok, Payment#'Payment'{cash_flow = CashFlow}};
apply_event(
    #'SessionStartedEvent'{session_id = SessionID},
    #'Payment'{sessions = Sessions} = Payment
) ->
    {ok, Payment#'Payment'{
        sessions = [
            #'Session'{
                id = SessionID,
                status = {pending, #'PendingStatus'{}},
                transaction = undefined
            }
            | Sessions
        ]
    }};
apply_event(
    #'SessionBoundEvent'{transaction = Transaction},
    #'Payment'{sessions = [LatestSession | Sessions]} = Payment
) ->
    {ok, Payment#'Payment'{
        sessions = [
            LatestSession#'Session'{transaction = Transaction} | Sessions
        ]
    }};
apply_event(
    #'SessionProcessedEvent'{},
    #'Payment'{sessions = [LatestSession | Sessions]} = Payment
) ->
    {ok, Payment#'Payment'{
        sessions = [
            LatestSession#'Session'{status = {complete, #'CompleteStatus'{}}}
            | Sessions
        ]
    }};
apply_event(
    #'SessionFailedEvent'{reason = Reason},
    #'Payment'{sessions = [LatestSession | Sessions]} = Payment
) ->
    {ok, Payment#'Payment'{
        sessions = [
            LatestSession#'Session'{
                status = {failed, #'FailedStatus'{reason = Reason}}
            }
            | Sessions
        ]
    }};
apply_event(#'PaymentFailedEvent'{reason = Reason}, Payment) ->
    {ok, Payment#'Payment'{status = {failed, #'FailedStatus'{reason = Reason}}}};
apply_event(#'PaymentCompletedEvent'{}, Payment) ->
    {ok, Payment#'Payment'{status = {complete, #'CompleteStatus'{}}}};
apply_event(Event, #'Payment'{} = Payment) ->
    {error, {unexpected, Event, Payment}}.

command(_Payment, _Context) ->
    {ok, undefined}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec payment_lifecycle_test_() -> [_].
payment_lifecycle_test_() ->
    [
        ?_assertEqual(
            #'Payment'{
                id = ~"payment-1",
                status = {complete, #'CompleteStatus'{}},
                payer = #'PayerID'{value = ~"payer"},
                merchant = #'MerchantID'{value = ~"merchant"},
                amount = money:'RUB'(100),
                route = #'Route'{
                    provider = #'ProviderID'{value = ~"merchant_provider"}
                },
                cash_flow = #'CashFlow'{
                    postings = [
                        #'CashFlowPosting'{
                            source = #'AccountID'{value = ~"payer_account"},
                            destination = #'AccountID'{
                                value = ~"merchant_provider_account"
                            },
                            amount = money:'RUB'(100)
                        }
                    ]
                },
                sessions = [
                    #'Session'{
                        id = ~"payment-1/1",
                        status = {complete, #'CompleteStatus'{}},
                        transaction = #'SessionTransaction'{
                            id = ~"payer/merchant_provider/100/RUB",
                            details = ~"stupid transaction's stupid details"
                        }
                    }
                ]
            },
            run(
                [
                    #'StartPaymentCommand'{
                        payment_id = ~"payment-1",
                        payer = #'PayerID'{value = ~"payer"},
                        merchant = #'MerchantID'{value = ~"merchant"},
                        amount = money:'RUB'(100)
                    },
                    #'FindRouteCommand'{},
                    #'CalculateCashFlowCommand'{},
                    #'StartSessionCommand'{},
                    #'BindSessionCommand'{},
                    #'ProcessSessionCommand'{},
                    #'CapturePaymentCommand'{}
                ],
                #{
                    fingy_accounter => ?MODULE,
                    fingy_config => ?MODULE,
                    fingy_provider => ?MODULE
                },
                goofy_context:new(~"payment", ~"1", goofy_util:unique())
            )
        )
    ].

%% Helpers

run(Commands, Opts, Context) ->
    {Payment, _} = lists:foldl(
        fun(Command, {P0, Ctx0}) ->
            Ctx1 = goofy_context:with_causation(goofy_util:unique(), Ctx0),
            case handle_command(Command, P0, Opts, Ctx1) of
                {ok, Events} ->
                    P1 = lists:foldl(
                        fun(Event, Acc0) ->
                            case apply_event(Event, Acc0) of
                                {ok, Acc1} ->
                                    Acc1;
                                {error, Reason} ->
                                    erlang:throw({event, {error, Reason}})
                            end
                        end,
                        P0,
                        Events
                    ),
                    {P1, Ctx1};
                {error, Reason} ->
                    erlang:throw({command, {error, Reason}})
            end
        end,
        {undefined,
            goofy_context:with_idempotency(goofy_util:unique(), Context)},
        Commands
    ),
    Payment.

%% Callback implementations

bind_transaction(
    #'PayerID'{value = PayerValue},
    #'ProviderID'{value = ProviderValue},
    #'Money'{amount = Amount, currency = Currency}
) ->
    {ok, #'SessionTransaction'{
        id =
            <<PayerValue/binary, "/", ProviderValue/binary, "/",
                (integer_to_binary(Amount))/binary, "/",
                (atom_to_binary(Currency))/binary>>,
        details = ~"stupid transaction's stupid details"
    }}.

process_transaction(#'SessionTransaction'{}) ->
    ok.

find_provider_for_merchant(#'MerchantID'{value = Value}) ->
    {ok, #'ProviderID'{value = <<Value/binary, "_provider">>}}.

get_payer_account(#'PayerID'{value = Value}) ->
    {ok, #'AccountID'{value = <<Value/binary, "_account">>}}.

get_provider_account(#'ProviderID'{value = Value}) ->
    {ok, #'AccountID'{value = <<Value/binary, "_account">>}}.

-endif.
