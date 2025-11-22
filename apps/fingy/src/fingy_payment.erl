-module(fingy_payment).

-behaviour(goofy).

-export([handle_command/4, apply_event/2]).

-export_type([payer/0]).
-export_type([merchant/0]).

-type opts() :: #{
    fingy_accounter := module(),
    fingy_config := module(),
    fingy_provder := module()
}.

-type id() :: binary().
-type payer() :: binary().
-type merchant() :: binary().
-type change() :: {
    Source :: fingy_accounter:account(),
    Destination :: fingy_accounter:account(),
    money:t()
}.

-record(provider_session, {
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
    sessions :: [#provider_session{}]
}).

-record(start_command, {
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

-spec handle_command(tuple(), undefined | #payment{}, opts(), goofy:context()) ->
    {ok, [tuple()]} | {error, term()}.
handle_command(
    #start_command{
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
        sessions =
            [#provider_session{status = LatestSessionStatus} | _] = Sessions
    },
    _Opts,
    _Context
) when LatestSessionStatus =/= pending ->
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
        sessions = [#provider_session{status = pending} | _]
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
            #provider_session{status = pending, transaction = Transaction} | _
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
        sessions = [#provider_session{status = complete} | _]
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
            #provider_session{
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
            LatestSession#provider_session{transaction = Transaction} | Sessions
        ]
    }};
apply_event(
    #session_processed_event{},
    #payment{sessions = [LatestSession | Sessions]} = Payment
) ->
    {ok, Payment#payment{
        sessions = [
            LatestSession#provider_session{status = complete} | Sessions
        ]
    }};
apply_event(
    #session_failed_event{reason = Reason},
    #payment{sessions = [LatestSession | Sessions]} = Payment
) ->
    {ok, Payment#payment{
        sessions = [
            LatestSession#provider_session{status = {failed, Reason}} | Sessions
        ]
    }};
apply_event(#payment_failed_event{reason = Reason}, Payment) ->
    {ok, Payment#payment{status = {failed, Reason}}};
apply_event(#payment_completed_event{}, Payment) ->
    {ok, Payment#payment{status = complete}};
apply_event(Event, #payment{} = Payment) ->
    {error, {unexpected, Event, Payment}}.
