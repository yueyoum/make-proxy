-module(mpc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% -define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
%%                                      permanent, 5000, Type, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

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
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    AcceptServer = {
            mpc_accept, {mpc_accept, start_link, []},
            permanent, 2000, worker, [mpc_accept]
            },
    ChildSupervisor = {
            mpc_child_sup, {mpc_child_sup, start_link, []},
            permanent, infinity, supervisor, [mpc_child_sup]
            },
    Restart = {one_for_one, 1, 1},
    {ok, {Restart, [ChildSupervisor, AcceptServer]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
