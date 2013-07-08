-module(mpc_child).

-behaviour(gen_server).

%% API
-export([start_link/1,
        accept/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {socket, remote}).
-define(TIMEOUT, 3600 * 24 * 1000).

-include("../../common/socks_type.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).

accept(Socket) ->
    mpc_child_sup:start_child(Socket).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Socket]) ->
    RemotePort = application:get_env(make_proxy_client, remote_port, 7071),
    RemoteAddr = application:get_env(make_proxy_client, remote_addr, "127.0.0.1"),
    {ok, Addr} = inet:getaddr(RemoteAddr, inet),

    case gen_tcp:connect(Addr, RemotePort, [binary, {active, false}, {reuseaddr, true}]) of
        {ok, RemoteSocket} ->
            {ok, Target} = find_target(Socket),
            ok = gen_tcp:send(RemoteSocket, transform:transform(Target)),
            IP = list_to_binary(tuple_to_list({127,0,0,1})),
            ok = gen_tcp:send(Socket, <<5, 0, 0, 1, IP/binary, 7070:16>>),
            inet:setopts(Socket, [{active, once}]),
            inet:setopts(RemoteSocket, [{active, once}]),
            {ok, #state{socket=Socket, remote=RemoteSocket}, ?TIMEOUT};
        {error, Error} ->
            {stop, Error}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({tcp, Socket, Request}, State) when Socket =:= State#state.socket ->
    case gen_tcp:send(State#state.remote, transform:transform(Request)) of
        ok ->
            inet:setopts(Socket, [{active, once}]),
            {noreply, State, ?TIMEOUT};
        {error, Error} ->
            {stop, Error, State}
    end;

handle_info({tcp, Socket, Response}, State) when Socket =:= State#state.remote ->
    case gen_tcp:send(State#state.socket, transform:transform(Response)) of
        ok ->
            inet:setopts(Socket, [{active, once}]),
            {noreply, State, ?TIMEOUT};
        {error, Error} ->
            {stop, Error, State}
    end;

handle_info({tcp_closed, _}, State) ->
    {stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
    {stop, Reason, State};

handle_info(timeout, State) ->
    {stop, timeout, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    gen_tcp:close(State#state.socket), 
    gen_tcp:close(State#state.remote),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



find_target(Socket) ->
    {ok, <<16#05:8, Nmethods:8>>} = gen_tcp:recv(Socket, 2),
    {ok, _Methods} = gen_tcp:recv(Socket, Nmethods),

    gen_tcp:send(Socket, <<16#05, 16#0>>),
    {ok, <<16#05:8, 16#01:8, _Rsv:8, AType:8>>} = gen_tcp:recv(Socket, 4),

    case AType of
        ?IPV4 ->
            {ok, <<Address:32>>} = gen_tcp:recv(Socket, 4),
            {ok, <<Port:16>>} = gen_tcp:recv(Socket, 2),
            {ok, <<?IPV4, Port:16, Address:32>>};
        ?IPV6 ->
            {ok, <<Address:128>>} = gen_tcp:recv(Socket, 16),
            {ok, <<Port:16>>} = gen_tcp:recv(Socket, 2),
            {ok, <<?IPV6, Port:16, Address:128>>};
        ?DOMAIN ->
            {ok, <<DomainLen:8>>} = gen_tcp:recv(Socket, 1),
            {ok, <<DomainBin/binary>>} = gen_tcp:recv(Socket, DomainLen),
            {ok, <<Port:16>>} = gen_tcp:recv(Socket, 2),
            {ok, <<?DOMAIN, Port:16, DomainLen:8, DomainBin/binary>>}
    end.


