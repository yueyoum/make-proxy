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
-export([start_link/0]).

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
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

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
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = infinity,
    Type = supervisor,

    Socks5AcceptorSup = {mpc_socks5_acceptor_sup, {mpc_socks5_acceptor_sup, start_link, []},
                        Restart, Shutdown, Type, [mpc_socks5_acceptor_sup]},

    HttpAcceptorSup = {mpc_http_acceptor_sup, {mpc_http_acceptor_sup, start_link, []},
                       Restart, Shutdown, Type, [mpc_http_acceptor_sup]},

    Socks5ChildSup = {mpc_socks5_child_sup, {mpc_socks5_child_sup, start_link, []},
                     Restart, Shutdown, Type, [mpc_socks5_child_sup]},

    HttpChildSup = {mpc_http_child_sup, {mpc_http_child_sup, start_link, []},
                    Restart, Shutdown, Type, [mpc_http_child_sup]},

    {ok, {SupFlags, [Socks5ChildSup, HttpChildSup, Socks5AcceptorSup, HttpAcceptorSup]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
