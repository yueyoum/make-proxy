-module(client).

-export([start/0]).

-export([start_process/1,
        accept/1]).

-include("utils.hrl").
-include("config.hrl").




start() ->
    {ok, Socket} = gen_tcp:listen(?LOCALPORT, ?OPTIONS(?GETADDR(?LOCALIP))),
    io:format("Porxy listen on ~p:~p~n", [?LOCALIP, ?LOCALPORT]),
    accept(Socket).


accept(Socket) ->
    {ok, Client} = gen_tcp:accept(Socket),
    spawn(?MODULE, start_process, [Client]),
    accept(Socket).



start_process(Client) ->
    case gen_tcp:connect(?GETADDR(?REMOTEIP), ?REMOTEPORT, ?OPTIONS) of
        {ok, RemoteSocket} ->
            communicate(Client, RemoteSocket);
        {error, Error} ->
            io:format("Connect error, ~p. ~p:~p~n", [Error, ?REMOTEIP, ?REMOTEPORT]),
            gen_tcp:close(Client),
            exit("connect error")
    end.



communicate(Client, RemoteSocket) ->
    Target = find_target(Client),


    IP = list_to_binary(tuple_to_list(?GETADDR(?LOCALIP))),
    ok = gen_tcp:send(Client, <<5, 0, 0, 1, IP/binary, ?LOCALPORT:16>>),

    case gen_tcp:recv(Client, 0, ?TIMEOUT) of
        {ok, Request} ->
            TargetLen = byte_size(Target),
            Data = <<TargetLen:16, Target/binary, Request/binary>>,
            ok = gen_tcp:send(RemoteSocket, Data),
            transfer(Client, RemoteSocket);
        {error, _Error} ->
            gen_tcp:close(RemoteSocket),
            gen_tcp:close(Client)
    end.





find_target(Client) ->
    {ok, <<16#05:8, Nmethods:8>>} = gen_tcp:recv(Client, 2),
    {ok, _Methods} = gen_tcp:recv(Client, Nmethods),

    gen_tcp:send(Client, <<16#05, 16#0>>),
    {ok, <<16#05:8, 16#01:8, _Rsv:8, AType:8>>} = gen_tcp:recv(Client, 4),

    case AType of
        ?IPV4 ->
            {ok, Address} = gen_tcp:recv(Client, 4),
            {ok, <<Port:16>>} = gen_tcp:recv(Client, 2),
            <<?IPV4, Port:16, Address/binary>>;
        ?DOMAIN ->
            {ok, <<DomainLen:8>>} = gen_tcp:recv(Client, 1),
            {ok, <<DomainBin/binary>>} = gen_tcp:recv(Client, DomainLen),
            {ok, <<Port:16>>} = gen_tcp:recv(Client, 2),
            <<?DOMAIN, Port:16, DomainBin/binary>>
    end.




transfer(Client, RemoteSocket) ->
    case gen_tcp:recv(RemoteSocket, 0) of
        {ok, Data} ->
            gen_tcp:send(Client, Data),
            transfer(Client, RemoteSocket);
        {error, _Error} ->
            gen_tcp:close(Client),
            gen_tcp:close(RemoteSocket)
    end.



