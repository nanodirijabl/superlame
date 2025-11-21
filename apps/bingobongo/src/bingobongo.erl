-module(bingobongo).

-behaviour(application).
-export([start/2, stop/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2]).

-behaviour(cowboy_handler).
-export([init/2, terminate/3]).

start(_StartType, _StartArgs) ->
    stdm:start_link(
        stdm:def(supervisor, #{
            flags => #{strategy => one_for_all},
            children => [
                stdm:def(gen_server, start_link, [
                    {local, bings_accountant}, ?MODULE, [], []
                ]),
                ranch:child_spec(
                    ?MODULE, ranch_tcp, [{port, 8080}], cowboy_clear, #{
                        env => #{
                            dispatch => cowboy_router:compile([
                                {'_', [{"/bing", ?MODULE, #{counter => 0}}]}
                            ])
                        }
                    }
                )
            ]
        })
    ).

stop(_State) ->
    ok.

%%

init(Req0, #{counter := Bings0} = State) ->
    Bings1 = Bings0 + 1,
    ok = report_bings(Bings1),
    Req1 = cowboy_req:reply(
        200,
        #{~"content-type" => ~"text/plain; charset=utf-8"},
        ~"""
        bong

        """,
        Req0
    ),
    {ok, Req1, State#{counter := Bings1}}.

terminate(_Reason, _Req, _State) ->
    ok.

%%

report_bings(Bings) ->
    gen_server:cast(bings_accountant, {bings, Bings}).

init([]) ->
    {ok, #{bings => undefined}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({bings, Bings}, State) ->
    {noreply, State#{bings := Bings}};
handle_cast(_Request, State) ->
    {noreply, State}.
