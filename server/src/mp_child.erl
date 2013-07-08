-module(mp_child).

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
    mp_child_sup:start_child(Socket).

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
    gen_server:cast(self(), sock_recv),
    {ok, #state{socket=Socket}, ?TIMEOUT}.

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
    {reply, Reply, State, ?TIMEOUT}.

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
handle_cast(sock_recv, #state{socket=Socket} = State) ->
    {ok, Remote} = connect_to_remote(Socket),
    inet:setopts(Socket, [{active, once}]),
    inet:setopts(Remote, [{active, once}]),
    {noreply, State#state{remote=Remote}, ?TIMEOUT}.



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
    case is_port(State#state.remote) of
        true -> gen_tcp:close(State#state.remote);
        false -> ok
    end,
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

connect_to_remote(Socket) ->
    {ok, Data} = gen_tcp:recv(Socket, 1),
    {ok, {Address, Port}} = parse_address(Socket, transform:transform(Data)),
    connect_target(Address, Port).


parse_address(Socket, AType) when AType =:= <<?IPV4>> ->
    {ok, Data} = gen_tcp:recv(Socket, 6),
    <<Port:16, Destination/binary>> = transform:transform(Data),
    Address = list_to_tuple( binary_to_list(Destination) ),
    {ok, {Address, Port}};

parse_address(Socket, AType) when AType =:= <<?IPV6>> ->
    {ok, Data} = gen_tcp:recv(Socket, 18),
    <<Port:16, Destination/binary>> = transform:transform(Data),
    Address = list_to_tuple( binary_to_list(Destination) ),
    {ok, {Address, Port}};


parse_address(Socket, AType) when AType =:= <<?DOMAIN>> ->
    {ok, Data} = gen_tcp:recv(Socket, 3),
    <<Port:16, DomainLen:8>> = transform:transform(Data),

    {ok, DataRest} = gen_tcp:recv(Socket, DomainLen),
    Destination = transform:transform(DataRest),

    Address = binary_to_list(Destination),
    {ok, {Address, Port}}.


connect_target(Address, Port) ->
    connect_target(Address, Port, 2).

connect_target(Address, Port, Times) ->
    case gen_tcp:connect(Address, Port, [binary, {active, false}], 5000) of
        {ok, TargetSocket} ->
            {ok, TargetSocket};
        {error, _Error} when Times >= 1 ->
            connect_target(Address, Port, Times-1);
        {error, Error} ->
            {error, Error}
    end.

