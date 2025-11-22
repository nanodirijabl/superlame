-module(fingy_payment).

-behaviour(goofy_entity).
-export([handle_command/4]).

-behaviour(goofy_projector).
-export([apply_event/2]).

-behaviour(goofy_commander).
-export([command/2]).

-export_type([payer/0]).
-export_type([merchant/0]).

-type opts() :: #{
    fingy_accounter := module(),
    fingy_config := module(),
    fingy_provider := module()
}.

-type id() :: binary().
-type payer() :: binary().
-type merchant() :: binary().
-type change() :: {
    Source :: fingy_accounter:account(),
    Destination :: fingy_accounter:account(),
    money:t()
}.

-record(session, {
    id :: binary(),
    status :: pending | complete | {failed, Reason :: term()},
    transaction :: undefined | fingy_provider:transaction()
}).
-record(route, {provider :: fingy_provider:id()}).
-record(payment, {
    id :: id(),
    status :: pending | complete | {failed, Reason :: term()},
    payer :: payer(),
    merchant :: merchant(),
    amount :: money:t(),
    route :: undefined | #route{},
    cash_flow :: undefined | nonempty_list(change()),
    sessions :: [#session{}]
}).

-record(start_payment_command, {
    id :: id(),
    payer :: payer(),
    merchant :: merchant(),
    amount :: money:t()
}).
-record(payment_started_event, {
    id :: id(),
    payer :: payer(),
    merchant :: merchant(),
    amount :: money:t()
}).

-record(find_route_command, {}).
-record(route_found_event, {route :: #route{}}).
-record(payment_failed_event, {reason :: term()}).

-record(calculate_cash_flow_command, {}).
-record(cash_flow_calculated_event, {cash_flow :: nonempty_list(change())}).

-record(start_session_command, {}).
-record(session_started_event, {session_id :: binary()}).

-record(bind_session_command, {}).
-record(session_bound_event, {transaction :: fingy_provider:transaction()}).
-record(session_failed_event, {reason :: term()}).

-record(process_session_command, {}).
-record(session_processed_event, {}).

-record(capture_payment_command, {}).
-record(payment_completed_event, {}).

-spec handle_command(
    tuple(), undefined | #payment{}, opts(), goofy_context:t()
) ->
    {ok, [tuple()]} | {error, term()}.
handle_command(
    #start_payment_command{
        id = ID,
        payer = Payer,
        merchant = Merchant,
        amount = Amount
    },
    undefined,
    _Opts,
    _Context
) ->
    {ok, [
        #payment_started_event{
            id = ID,
            payer = Payer,
            merchant = Merchant,
            amount = Amount
        }
    ]};
handle_command(
    #find_route_command{},
    #payment{status = pending, merchant = Merchant, route = undefined},
    Opts,
    _Context
) ->
    ConfigMod = maps:get(fingy_config, Opts),
    case fingy_config:find_provider_for_merchant(ConfigMod, Merchant) of
        {ok, ProviderID} ->
            {ok, [#route_found_event{route = #route{provider = ProviderID}}]};
        {error, not_found} ->
            {ok, [#payment_failed_event{reason = {route, not_found}}]}
    end;
handle_command(
    #calculate_cash_flow_command{},
    #payment{
        status = pending,
        payer = Payer,
        amount = Amount,
        route = #route{provider = ProviderID}
    },
    Opts,
    _Context
) ->
    AccounterMod = maps:get(fingy_accounter, Opts),
    CashFlow = [
        {
            fingy_accounter:get_payer_account(AccounterMod, Payer),
            fingy_accounter:get_provider_account(AccounterMod, ProviderID),
            Amount
        }
    ],
    {ok, [#cash_flow_calculated_event{cash_flow = CashFlow}]};
handle_command(
    #start_session_command{},
    #payment{
        id = PaymentID,
        status = pending,
        sessions = Sessions
    },
    _Opts,
    _Context
) when
    length(Sessions) =:= 0 orelse (hd(Sessions))#session.status =/= pending
->
    SessionID =
        <<PaymentID/binary, "/",
            (integer_to_binary(length(Sessions) + 1))/binary>>,
    {ok, [#session_started_event{session_id = SessionID}]};
handle_command(
    #bind_session_command{},
    #payment{
        status = pending,
        payer = Payer,
        amount = Amount,
        route = #route{provider = ProviderID},
        sessions = [#session{status = pending} | _]
    },
    Opts,
    _Context
) ->
    ProviderMod = maps:get(fingy_provider, Opts),
    case
        fingy_provider:bind_transaction(ProviderMod, Payer, ProviderID, Amount)
    of
        {ok, Transaction} ->
            {ok, [#session_bound_event{transaction = Transaction}]};
        {error, Reason} ->
            {ok, [#session_failed_event{reason = Reason}]}
    end;
handle_command(
    #process_session_command{},
    #payment{
        status = pending,
        sessions = [
            #session{status = pending, transaction = Transaction} | _
        ]
    },
    Opts,
    _Context
) ->
    ProviderMod = maps:get(fingy_provider, Opts),
    case fingy_provider:process_transaction(ProviderMod, Transaction) of
        ok ->
            {ok, [#session_processed_event{}]};
        {error, Reason} ->
            {ok, [#session_failed_event{reason = Reason}]}
    end;
handle_command(
    #capture_payment_command{},
    #payment{
        status = pending,
        sessions = [#session{status = complete} | _]
    },
    _Opts,
    _Context
) ->
    {ok, [#payment_completed_event{}]};
handle_command(Command, Payment, _Opts, _Context) ->
    {error, {unexpected, Command, Payment}}.

apply_event(
    #payment_started_event{
        id = ID,
        payer = Payer,
        merchant = Merchant,
        amount = Amount
    },
    undefined
) ->
    {ok, #payment{
        id = ID,
        status = pending,
        payer = Payer,
        merchant = Merchant,
        amount = Amount,
        route = undefined,
        cash_flow = undefined,
        sessions = []
    }};
apply_event(#route_found_event{route = Route}, Payment) ->
    {ok, Payment#payment{route = Route}};
apply_event(#cash_flow_calculated_event{cash_flow = CashFlow}, Payment) ->
    {ok, Payment#payment{cash_flow = CashFlow}};
apply_event(
    #session_started_event{session_id = SessionID},
    #payment{sessions = Sessions} = Payment
) ->
    {ok, Payment#payment{
        sessions = [
            #session{
                id = SessionID,
                status = pending,
                transaction = undefined
            }
            | Sessions
        ]
    }};
apply_event(
    #session_bound_event{transaction = Transaction},
    #payment{sessions = [LatestSession | Sessions]} = Payment
) ->
    {ok, Payment#payment{
        sessions = [
            LatestSession#session{transaction = Transaction} | Sessions
        ]
    }};
apply_event(
    #session_processed_event{},
    #payment{sessions = [LatestSession | Sessions]} = Payment
) ->
    {ok, Payment#payment{
        sessions = [
            LatestSession#session{status = complete} | Sessions
        ]
    }};
apply_event(
    #session_failed_event{reason = Reason},
    #payment{sessions = [LatestSession | Sessions]} = Payment
) ->
    {ok, Payment#payment{
        sessions = [
            LatestSession#session{status = {failed, Reason}} | Sessions
        ]
    }};
apply_event(#payment_failed_event{reason = Reason}, Payment) ->
    {ok, Payment#payment{status = {failed, Reason}}};
apply_event(#payment_completed_event{}, Payment) ->
    {ok, Payment#payment{status = complete}};
apply_event(Event, #payment{} = Payment) ->
    {error, {unexpected, Event, Payment}}.

command(_Payment, _Context) ->
    {ok, undefined}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec payment_lifecycle_test_() -> [_].
payment_lifecycle_test_() ->
    Opts = #{
        fingy_accounter => fingy_accounter_stupid,
        fingy_config => fingy_config_stupid,
        fingy_provider => fingy_provider_stupid
    },
    Context = goofy_context:new(~"payment", ~"1", goofy_util:unique()),
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
        [
            #start_payment_command{
                id = ~"payment-1",
                payer = ~"payer",
                merchant = ~"merchant",
                amount = {100, ~"RUB"}
            },
            #find_route_command{},
            #calculate_cash_flow_command{},
            #start_session_command{},
            #bind_session_command{},
            #process_session_command{},
            #capture_payment_command{}
        ]
    ),
    [
        ?_assertEqual(
            #payment{
                id = ~"payment-1",
                status = complete,
                payer = ~"payer",
                merchant = ~"merchant",
                amount = money:'RUB'(100),
                route = #route{provider = ~"merchant_provider"},
                cash_flow = [
                    {
                        ~"payer_account",
                        ~"merchant_provider_account",
                        money:'RUB'(100)
                    }
                ],
                sessions = [
                    #session{
                        id = ~"payment-1/1",
                        status = complete,
                        transaction = {
                            ~"payer/merchant_provider/100/RUB",
                            ~"stupid transaction's stupid details"
                        }
                    }
                ]
            },
            Payment
        )
    ].

-endif.
