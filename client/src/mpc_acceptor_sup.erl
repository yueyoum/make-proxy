%%%-------------------------------------------------------------------
%%% @author wang <yueyoum@gmail.com>
%%% @copyright (C) 2014
%%% @doc
%%%
%%% @end
%%% Created : 2014-12-18 13:01
%%%-------------------------------------------------------------------
-module(mpc_acceptor_sup).
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
    Ip = {0, 0, 0, 0},
    {ok, Port} = application:get_env(make_proxy_client, local_port),
    {ok, LSock} = gen_tcp:listen(Port, [binary,
        {ip, Ip},
        {reuseaddr, true},
        {active, false},
        {backlog, 256}]),

    supervisor:start_link({local, ?SERVER}, ?MODULE, [LSock]).

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
init([LSock]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    Fun = fun(Index) ->
        Id = list_to_atom("mpc_acceptor-" ++ integer_to_list(Index)),
        {Id, {mpc_acceptor, start_link, [LSock]},
        Restart, Shutdown, Type, [mpc_acceptor]}
        end,

    Children = [Fun(Index) || Index <- lists:seq(1, 10)],

    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
