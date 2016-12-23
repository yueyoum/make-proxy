%%%-------------------------------------------------------------------
%%% @author wang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Oct 2016 下午6:30
%%%-------------------------------------------------------------------
-module(mp_client_worker).
-author("wang").

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

-include("mp_client.hrl").


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
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
-spec(init(Args :: term()) ->
    {ok, State :: #client{}} | {ok, State :: #client{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([Ref, Socket, Transport, _Opts]) ->
    put(init, true),
    {ok, Key} = application:get_env(make_proxy, key),
    {OK, Closed, Error} = Transport:messages(),

    ok = Transport:setopts(Socket, [binary, {active, once}, {packet, raw}]),

    State = #client{key = Key, ref = Ref, socket = Socket,
        transport = Transport, ok = OK, closed = Closed,
        error = Error, buffer = <<>>, keep_alive = false},

    {ok, State, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #client{}) ->
    {reply, Reply :: term(), NewState :: #client{}} |
    {reply, Reply :: term(), NewState :: #client{}, timeout() | hibernate} |
    {noreply, NewState :: #client{}} |
    {noreply, NewState :: #client{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #client{}} |
    {stop, Reason :: term(), NewState :: #client{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #client{}) ->
    {noreply, NewState :: #client{}} |
    {noreply, NewState :: #client{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #client{}}).
handle_cast(_Request, State) ->
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
-spec(handle_info(Info :: timeout() | term(), State :: #client{}) ->
    {noreply, NewState :: #client{}} |
    {noreply, NewState :: #client{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #client{}}).

handle_info({OK, Socket, Data},
    #client{socket = Socket, transport = Transport, ok = OK, protocol = undefined} = State) ->

    case detect_protocol(Data) of
        {ok, ProtocolHandler} ->
            State1 = State#client{protocol = ProtocolHandler},
            case ProtocolHandler:request(Data, State1) of
                {ok, State2} ->
                    ok = Transport:setopts(Socket, [{active, once}]),
                    {noreply, State2};
                {error, Reason} ->
                    {stop, Reason, State1}
            end;
        {error, Reason} ->
            {stop, Reason, State}
    end;

handle_info({OK, Socket, Data},
    #client{socket = Socket, transport = Transport, ok = OK, protocol = Protocol} = State) ->
    case Protocol:request(Data, State) of
        {ok, State1} ->
            ok = Transport:setopts(Socket, [{active, once}]),
            {noreply, State1};
        {error, Reason} ->
            {stop, Reason, State}
    end;

handle_info({tcp, Remote, Data},
    #client{key = Key, socket = Socket, transport = Transport, remote = Remote} = State) ->
    {ok, RealData} = mp_crypto:decrypt(Key, Data),
    ok = Transport:send(Socket, RealData),
    ok = inet:setopts(Remote, [{active, once}]),
    {noreply, State};

handle_info({Closed, _}, #client{closed = Closed} = State) ->
    {stop, normal, State};

handle_info({Error, _, Reason}, #client{error = Error} = State) ->
    {stop, Reason, State};

handle_info({tcp_closed, _}, State) ->
    {stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
    {stop, Reason, State};

handle_info(timeout, #client{ref = Ref} = State) ->
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
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #client{}) -> term()).
terminate(_Reason, #client{socket = Socket, transport = Transport, remote = Remote}) ->
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
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #client{},
    Extra :: term()) ->
    {ok, NewState :: #client{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


-spec detect_protocol(binary()) -> {ok, module()} | {error, term()}.
detect_protocol(<<Head:8, _Rest/binary>>) ->
    Protocols = [mp_client_http, mp_client_socks],
    do_detect_protocol(Head, Protocols);

detect_protocol(_) ->
    {error, invalid_data}.

-spec do_detect_protocol(byte(), list()) -> {ok, module()} | {error, term()}.
do_detect_protocol(_, []) ->
    {error, no_protocol_handler};

do_detect_protocol(Head, [P | Ps]) ->
    case P:detect_head(Head) of
        true ->
            {ok, P};
        false ->
            do_detect_protocol(Head, Ps)
    end.
