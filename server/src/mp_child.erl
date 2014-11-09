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

-record(state, {key, lsock, socket, remote}).

-include("../../common/socks_type.hrl").

-define(TIMEOUT, 1000 * 60 * 10).


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
    {ok, Key} = application:get_env(make_proxy_server, key),
    {ok, #state{key=Key, lsock=LSock}, 0}.

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

%% the first message send to this child
handle_info(timeout, #state{key=Key, lsock=LSock, socket=undefined} = State) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    mp_sup:start_child(),
    case connect_to_remote(Socket, Key) of
        {ok, Remote} ->
            inet:setopts(Socket, [{active, once}]),
            inet:setopts(Remote, [{active, once}]),
            {noreply, State#state{socket=Socket, remote=Remote}, ?TIMEOUT};
        {error, Error} ->
            {stop, Error, State}
    end;

%% send by OPT timeout
handle_info(timeout, #state{socket=Socket} = State) when is_port(Socket) ->
    {stop, timeout, State};

%% recv from client, and send to server
handle_info({tcp, Socket, Request}, #state{key=Key, socket=Socket, remote=Remote} = State) ->
    case gen_tcp:send(Remote, mp_crypto:decrypt(Key, Request)) of
        ok ->
            inet:setopts(Socket, [{active, once}]),
            {noreply, State, ?TIMEOUT};
        {error, Error} ->
            {stop, Error, State}
    end;

%% recv from server, and send back to client
handle_info({tcp, Socket, Response}, #state{key=Key, socket=Client, remote=Socket} = State) ->
    case gen_tcp:send(Client, mp_crypto:encrypt(Key, Response)) of
        ok ->
            inet:setopts(Socket, [{active, once}]),
            {noreply, State, ?TIMEOUT};
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
terminate(_Reason, #state{socket=Socket, remote=Remote}) ->
    case is_port(Socket) of
        true -> gen_tcp:close(Socket);
        false -> ok
    end,

    case is_port(Remote) of
        true -> gen_tcp:close(Remote);
        false -> ok
    end.

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

connect_to_remote(Socket, Key) ->
    {ok, EntrypedData} = gen_tcp:recv(Socket, 0),
    RealData = mp_crypto:decrypt(Key, EntrypedData),
    <<AType:8, Rest/binary>> = RealData,

    {ok, {Address, Port}} = parse_address(AType, Rest),
    connect_target(Address, Port).


parse_address(?IPV4, Data) ->
    <<Port:16, Destination/binary>> = Data,
    Address = list_to_tuple( binary_to_list(Destination) ),
    {ok, {Address, Port}};

parse_address(?IPV6, Data) ->
    <<Port:16, Destination/binary>> = Data,
    Address = list_to_tuple( binary_to_list(Destination) ),
    {ok, {Address, Port}};


parse_address(?DOMAIN, Data) ->
    <<Port:16, _DomainLen:8, Rest/binary>> = Data,
    Address = binary_to_list(Rest),
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

