-module(mpc_http_child).

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

-record(state, {key, socket, remote}).

-include("../../include//socks_type.hrl").
-define(TIMEOUT, 1000 * 60 * 10).
-define(HTTP_METHOD_HEADER, [
    <<"GE">>,   % GET
    <<"PO">>,   % POST
    <<"HE">>,   % HEAD
    <<"PU">>,   % PUT
    <<"DE">>,   % DELETE
    <<"TR">>,   % TRACE
    <<"CO">>,   % CONNECT
    <<"OP">>    % OPTIONS
]).

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
    {ok, Key} = application:get_env(make_proxy_client, key),
    {ok, #state{key=Key, socket=Socket}}.

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
handle_cast(_Info, State) ->
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

%% send by OTP timeout
handle_info(timeout, #state{remote =  Remote} = State) when is_port(Remote) ->
    {stop, timeout, State};

%% recv from client, and send to remote
handle_info({tcp, Socket, Request}, #state{key=Key, socket=Socket, remote=Remote} = State) ->
    io:format("Client Got: ~p~n", [Request]),

    <<Header:2/binary, _Rest/binary>> = Request,

    {ok, NewRemote} =
    case lists:member(Header, ?HTTP_METHOD_HEADER) of
        true ->
            % New Domain Request
            case is_port(Remote) of
                true ->
                    io:format("New Request in an OLD Socket!!!~n", []),
                    gen_tcp:close(Remote);
                false ->
                    ok
            end,

            {ok, R} = connect_to_remote(),
            {ok, Target} = find_target(Request),
            ok = gen_tcp:send(R, mp_crypto:encrypt(Key, Target)),
            {ok, R};
        false ->
            case is_port(Remote) of
                true ->
                    {ok, Remote};
                false ->
                    {ok, R} = connect_to_remote(),
                    {ok, Target} = find_target(Request),
                    ok = gen_tcp:send(R, mp_crypto:encrypt(Key, Target)),
                    {ok, R}
            end
    end,


    case gen_tcp:send(NewRemote, mp_crypto:encrypt(Key, Request)) of
        ok ->
            inet:setopts(Socket, [{active, once}]),
            {noreply, State#state{remote = NewRemote}, ?TIMEOUT};
        {error, Error} ->
            {stop, Error, State}
    end;

%% recv from remote, and send back to client
handle_info({tcp, Socket, Response}, #state{key=Key, socket=Client, remote=Socket} = State) ->
    {ok, RealData} = mp_crypto:decrypt(Key, Response),
    case gen_tcp:send(Client, RealData) of
        ok ->
            inet:setopts(Socket, [{active, once}]),
            {noreply, State, ?TIMEOUT};
        {error, Error} ->
            {stop, Error, State}
    end;

handle_info({tcp_closed, Socket}, #state{socket = Client, remote = Remote} = State) ->
    case Socket /= Client andalso Socket /= Remote of
        true ->
            % old connection
            {noreply, State, ?TIMEOUT};
        false ->
            {stop, normal, State}
    end;


handle_info({tcp_error, Socket, Reason}, #state{socket = Client, remote = Remote} = State) ->
    case Socket /= Client andalso Socket /= Remote of
        true ->
            % old connection
            {noreply, State, ?TIMEOUT};
        false ->
            {stop, Reason, State}
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

connect_to_remote() ->
    {ok, RemoteAddr} = application:get_env(make_proxy_client, remote_addr),
    {ok, RemotePort} = application:get_env(make_proxy_client, remote_port),
    {ok, Addr} = inet:getaddr(RemoteAddr, inet),

    gen_tcp:connect(Addr, RemotePort, [binary, {active, once}, {packet, 4}]).


%% http proxy
find_target(Request) ->
    case find_target_of_http_request(Request) of
        {ok, {Host, Port}} ->
            DomainLen = length(Host),
            Domain = list_to_binary(Host),
            {ok, <<?DOMAIN, Port:16, DomainLen:8, Domain/binary>>};
        {Error, Reason} ->
            {Error, Reason}
    end.


find_target_of_http_request(Request) ->
    case binary:split(Request, <<"\r\n">>) of
        [Request] ->
            % Need recv more...
            {more, Request};
        [RequestHeader, _Rest] ->
            [_Method, Uri, _Version] = binary:split(RequestHeader, <<" ">>, [global]),
            case parse_http_request(Uri) of
                {ok, {Host, Port}} ->
                    {ok, {Host, Port}};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

parse_http_request(Uri) ->
    case http_uri:parse(binary_to_list(Uri)) of
        {ok, {_Sheme, _UserInfo, Host, Port, _Path, _Query}} ->
            {ok, {Host, Port}};
        {ok, {_Sheme, _UserInfo, Host, Port, _Path, _Query, _Fragment}} ->
            {ok, {Host, Port}};
%%         {error, {malformed_url, _, HostWithPort}} ->
%%             % XXX workaround
%%             [Host, Port] = string:tokens(HostWithPort, ":"),
%%             {ok, {Host, list_to_integer(Port)}};
        {error, Reason} ->
            {error, Reason}
    end.
