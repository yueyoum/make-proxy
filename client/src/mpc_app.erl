-module(mpc_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    {ok, Socsk5Port} = application:get_env(make_proxy_client, local_socks5_port),
    {ok, Socks5LSock} = gen_tcp:listen(Socsk5Port, [binary,
        {ip, {0, 0, 0, 0}},
        {reuseaddr, true},
        {active, false},
        {backlog, 256}]),

    {ok, HttpPort} = application:get_env(make_proxy_client, local_http_port),
    {ok, HttpLSock} = gen_tcp:listen(HttpPort, [binary,
        {ip, {0, 0, 0, 0}},
        {reuseaddr, true},
        {active, once},
        {backlog, 256}]),

    case mpc_sup:start_link([Socks5LSock, HttpLSock]) of
        {ok, Pid} ->
            mpc_socks5_sup:start_child(),
            mpc_http_sup:start_child(),
            {ok, Pid};
        Other ->
            {error, Other}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
