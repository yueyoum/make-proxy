-module(client_erlang).

-export([start/0]).

-export([
    start_process/1,
    local_process/2,
    remote_process/3,
    local_transport/2,
    remote_transport/2
]).


-define(IPV4, 16#01).
-define(IPV6, 16#04).
-define(DOMAIN, 16#03).



cover_to_ip_address(IPStr) ->
    {ok, IP} = inet:getaddr(IPStr, inet),
    IP.

get_bind_port() ->
    %% FIXME
    7070.




local_process(Local, ParentPid) ->
    case gen_tcp:recv(Local, 0) of
        {ok, Data} ->
            ParentPid ! {self(), {request, Data}},
            local_transport(Local, ParentPid);
        {error, Error} ->
            io:format("local recv error: ~p~n", [Error]),
            exit("local recv error ~p~n", [Error])
    end.



local_transport(Local, ParentPid) ->
    receive
        {response, Response} ->
            gen_tcp:send(Local, Response),
            local_transport(Local, ParentPid)
    end,
    local_process(Local, ParentPid).





remote_process(Address, Port, ParentPid) ->
    Options = [
            inet,
            binary,
            {active, false},
            {nodelay, true}
        ],

    case gen_tcp:connect(Address, Port, Options, 5000) of
        {ok, Remote} ->
            io:format("connect done...~n", []),
            remote_process(Remote, ParentPid);
        {error, Error} ->
            io:format("remote connect error~p~n", [Error]),
            exit("remote connect error ~p~n", [Error])
    end.


remote_process(Remote, ParentPid) ->
    receive
        {request, Request} ->
            case gen_tcp:send(Remote, Request) of
                ok -> 
                    remote_transport(Remote, ParentPid);
                {error, Error} -> 
                    io:format("remote send error: ~p~n", [Error]),
                    exit("remote send error")
            end
    end.



remote_transport(Remote, ParentPid) ->
    case gen_tcp:recv(Remote, 0) of
        {ok, Data} ->
            ParentPid ! {self(), {response, Data}},
            remote_transport(Remote, ParentPid);
        {error, Error} ->
            io:format("remote recv error: ~p~n", [Error]),
            exit("remote recv error: ~p~n", [Error])
    end.
    % remote_process(Remote, ParentPid).





start() ->
    Port = get_bind_port(),
    Options = [
            inet,
            binary,
            {ip, cover_to_ip_address("127.0.0.1")},
            {reuseaddr, true},
            {active, false},
            {nodelay, true}
        ],

    {ok, Socket} = gen_tcp:listen(Port, Options),

    io:format("Listen on ~p~n", [Port]),
    accept(Socket).


accept(Socket) ->
    {ok, Client} = gen_tcp:accept(Socket),
    spawn(?MODULE, start_process, [Client]),
    accept(Socket).



start_process(Client) ->
    io:format("~nstart process: ~p~n", [Client]),

    {Type, Destination, Port} = find_target(Client),

    Address =
    case Type of
        ?IPV4 ->
            list_to_tuple( binary_to_list(Destination) );
        ?DOMAIN ->
            binary_to_list(Destination)
    end,

    io:format("Address: ~p, Port: ~p~n", [Address, Port]),
    BindPort = get_bind_port(),
    BackData = <<5, 0, 0, 1, <<127,0,0,1>>/binary, BindPort:16>>,
    gen_tcp:send(Client, BackData),


    LocalPid = spawn_link(?MODULE, local_process, [Client, self()]),
    RemotePid = spawn_link(?MODULE, remote_process, [Address, Port, self()]),
    local_remote_bridge(LocalPid, RemotePid).


local_remote_bridge(LocalPid, RemotePid) ->
    receive
        {LocalPid, Msg} -> RemotePid ! Msg;
        {RemotePid, Msg} -> LocalPid ! Msg
    end,
    local_remote_bridge(LocalPid, RemotePid).





find_target(Client) ->
    {ok, <<16#05:8, Nmethods:8>>} = gen_tcp:recv(Client, 2),
    {ok, _Methods} = gen_tcp:recv(Client, Nmethods),

    gen_tcp:send(Client, <<16#05, 16#0>>),
    {ok, <<16#05:8, 16#01:8, _Rsv:8, AType:8>>} = gen_tcp:recv(Client, 4),

    Target =
    case AType of
        ?IPV4 ->
            {ok, Address} = gen_tcp:recv(Client, 4),
            {ok, <<Port:16>>} = gen_tcp:recv(Client, 2),
            {?IPV4, Address, Port};
        ?DOMAIN ->
            {ok, <<DomainLen:8>>} = gen_tcp:recv(Client, 1),
            {ok, <<DomainBin/binary>>} = gen_tcp:recv(Client, DomainLen),
            {ok, <<Port:16>>} = gen_tcp:recv(Client, 2),
            {?DOMAIN, DomainBin, Port}
    end,
    io:format("Target: ~p~n", [Target]),
    Target.


