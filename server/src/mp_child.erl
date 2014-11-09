-module(mp_child).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {lsock, socket, remote}).

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
start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).


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
init([LSock]) ->
    io:format("mp_child, init...~n", []),
    {ok, #state{lsock=LSock}, 0}.

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

handle_info(timeout, #state{lsock=LSock} = State) ->
    io:format("Start Accept...~p~n", [self()]),
    {ok, Socket} = gen_tcp:accept(LSock),
    io:format("Accept success...~p~n", [self()]),
    mp_sup:start_child(),
    case connect_to_remote(Socket) of
        {ok, Remote} ->
            inet:setopts(Socket, [{active, once}]),
            inet:setopts(Remote, [{active, once}]),
            {noreply, State#state{socket=Socket, remote=Remote}};
        {error, Error} ->
            {stop, Error, State}
    end;


%% recv from client, and send to server
handle_info({tcp, Socket, Request}, #state{socket=Socket, remote=Remote} = State) ->
    case gen_tcp:send(Remote, Request) of
        ok ->
            inet:setopts(Socket, [{active, once}]),
            {noreply, State};
        {error, Error} ->
            {stop, Error, State}
    end;

%% recv from server, and send back to client
handle_info({tcp, Socket, Response}, #state{socket=Client, remote=Socket} = State) ->
    case gen_tcp:send(Client, Response) of
        ok ->
            inet:setopts(Socket, [{active, once}]),
            {noreply, State};
        {error, Error} ->
            {stop, Error, State}
    end;

handle_info({tcp_closed, _}, State) ->
    {stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
    {stop, Reason, State}.



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
    {ok, AType} = gen_tcp:recv(Socket, 1),
    {ok, {Address, Port}} = parse_address(Socket, AType),
    connect_target(Address, Port).


parse_address(Socket, AType) when AType =:= <<?IPV4>> ->
    {ok, Data} = gen_tcp:recv(Socket, 6),
    <<Port:16, Destination/binary>> = Data,
    Address = list_to_tuple( binary_to_list(Destination) ),
    {ok, {Address, Port}};

parse_address(Socket, AType) when AType =:= <<?IPV6>> ->
    {ok, Data} = gen_tcp:recv(Socket, 18),
    <<Port:16, Destination/binary>> = Data,
    Address = list_to_tuple( binary_to_list(Destination) ),
    {ok, {Address, Port}};


parse_address(Socket, AType) when AType =:= <<?DOMAIN>> ->
    {ok, Data} = gen_tcp:recv(Socket, 3),
    <<Port:16, DomainLen:8>> = Data,

    {ok, DataRest} = gen_tcp:recv(Socket, DomainLen),
    Destination = DataRest,

    Address = binary_to_list(Destination),
    {ok, {Address, Port}}.


connect_target(Address, Port) ->
    connect_target(Address, Port, 2).

connect_target(_, _, 0) ->
    {error, connect_failure};

connect_target(Address, Port, RetryTimes) ->
    case gen_tcp:connect(Address, Port, [binary, {active, false}], 5000) of
        {ok, TargetSocket} ->
            {ok, TargetSocket};
        {error, _Error} ->
            connect_target(Address, Port, RetryTimes-1)
    end.

