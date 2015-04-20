%%%-------------------------------------------------------------------
%%% @author wang <yueyoum@gmail.com>
%%% @copyright (C) 2014
%%% @doc
%%%
%%% @end
%%% Created : 2014-12-18 13:00
%%%-------------------------------------------------------------------
-module(mpc_sup).
-author("wang").

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link([port()]) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link([Socks5LSock, HttpLSock]) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Socks5LSock, HttpLSock]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
        MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
        [ChildSpec :: supervisor:child_spec()]
    }} |
    ignore |
    {error, Reason :: term()}).
init([Socks5LSock, HttpLSock]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = infinity,
    Type = supervisor,

    Socks5Sup = {mpc_socks5_sup, {mpc_socks5_sup, start_link, [Socks5LSock]},
                        Restart, Shutdown, Type, [mpc_socks5_sup]},

    HttpSup = {mpc_http_sup, {mpc_http_sup, start_link, [HttpLSock]},
                       Restart, Shutdown, Type, [mpc_http_sup]},

    {ok, {SupFlags, [Socks5Sup, HttpSup]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
