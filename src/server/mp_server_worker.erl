-module(mp_server_worker).

-behaviour(gen_server).
-behaviour(ranch_protocol).

%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-record(state, {
    key :: string(),
    ref :: ranch:ref(),
    socket :: any(),
    transport :: module(),
    ok,
    closed,
    error,
    remote :: gen_tcp:socket() | undefined
}).


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
start_link(Ref, Socket, Transport, Opts) ->
    gen_server:start_link(?MODULE, [Ref, Socket, Transport, Opts], []).


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
init([Ref, Socket, Transport, _Opts]) ->
    put(init, true),
    {ok, Key} = application:get_env(make_proxy, key),
    {OK, Closed, Error} = Transport:messages(),

    ok = Transport:setopts(Socket, [{active, once}, {packet, 4}]),

    State = #state{key = Key, ref = Ref, socket = Socket,
        transport = Transport, ok = OK, closed = Closed,
        error = Error},

    {ok, State, 0}.

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

%% first message from client
handle_info({OK, Socket, Request},
    #state{key = Key, socket = Socket,
        transport = Transport, ok = OK, remote = undefined} = State) ->

    case connect_to_remote(Request, Key) of
        {ok, Remote} ->
            ok = Transport:setopts(Socket, [{active, once}]),
            {noreply, State#state{remote = Remote}, ?TIMEOUT};
        {error, Error} ->
            {stop, Error, State}
    end;


%% recv from client, then send to server
handle_info({OK, Socket, Request},
    #state{key = Key, socket = Socket,
        transport = Transport, ok = OK, remote = Remote} = State) ->

    {ok, RealData} = mp_crypto:decrypt(Key, Request),

    case gen_tcp:send(Remote, RealData) of
        ok ->
            ok = Transport:setopts(Socket, [{active, once}]),
            {noreply, State, ?TIMEOUT};
        {error, Error} ->
            {stop, Error, State}
    end;


%% recv from server, and send back to client
handle_info({tcp, Remote, Response},
    #state{key = Key, socket = Client,
        transport = Transport, remote = Remote} = State) ->

    case Transport:send(Client, mp_crypto:encrypt(Key, Response)) of
        ok ->
            ok = inet:setopts(Remote, [{active, once}]),
            {noreply, State, ?TIMEOUT};
        {error, Error} ->
            {stop, Error, State}
    end;

handle_info({Closed, _}, #state{closed = Closed} = State) ->
    {stop, normal, State};

handle_info({Error, _, Reason}, #state{error = Error} = State) ->
    {stop, Reason, State};

handle_info({tcp_closed, _}, State) ->
    {stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
    {stop, Reason, State};

handle_info(timeout, #state{ref = Ref} = State) ->
    case get(init) of
        true ->
            % init
            ok = ranch:accept_ack(Ref),
            erase(init),
            {noreply, State};
        undefined ->
            % timeout
            {stop, normal, State}
    end.


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
terminate(_Reason, #state{socket = Socket, transport = Transport, remote = Remote}) ->
    case is_port(Socket) of
        true -> Transport:close(Socket);
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

-spec connect_to_remote(binary(), nonempty_string()) ->
    {ok, inet:socket()} | {error, term()}.
connect_to_remote(Data, Key) ->
    case mp_crypto:decrypt(Key, Data) of
        {ok, RealData} ->
            {Address, Port} = binary_to_term(RealData),
            connect_target(Address, Port);
        {error, Error} ->
            {error, Error}
    end.


-spec connect_target(inet:ip_address(), inet:port_number()) ->
    {ok, inet:socket()} | {error, term()}.
connect_target(Address, Port) ->
    connect_target(Address, Port, 2).

connect_target(_, _, 0) ->
    {error, connect_failure};

connect_target(Address, Port, RetryTimes) ->
    case gen_tcp:connect(Address, Port, [binary, {active, once}], 5000) of
        {ok, TargetSocket} ->
            {ok, TargetSocket};
        {error, _Error} ->
            connect_target(Address, Port, RetryTimes - 1)
    end.
