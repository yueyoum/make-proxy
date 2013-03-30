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
    Pid = spawn(?MODULE, start_process, [Client]),
    ok = gen_tcp:controlling_process(Client, Pid),
    accept(Socket).



start_process(Client) ->
    case gen_tcp:connect(?GETADDR(?REMOTEIP), ?REMOTEPORT, ?OPTIONS) of
        {ok, RemoteSocket} ->
            ok = inet:setopts(RemoteSocket, [{active, true}]),
            communicate(Client, RemoteSocket);
        {error, Error} ->
            io:format("Connect error, ~p. ~p:~p~n", [Error, ?REMOTEIP, ?REMOTEPORT]),
            gen_tcp:close(Client),
            exit("connect error")
    end.



communicate(Client, RemoteSocket) ->
    try
        Target = find_target(Client),
        ok = inet:setopts(Client, [{active, true}]),

        ok = gen_tcp:send(RemoteSocket, transform:transform(Target)),


        IP = list_to_binary(tuple_to_list(?GETADDR(?LOCALIP))),
        ok = gen_tcp:send(Client, <<5, 0, 0, 1, IP/binary, ?LOCALPORT:16>>)
    catch
        Error:Reason ->
            io:format("communicate error, ~p: ~p~n", [Error, Reason]),
            gen_tcp:close(RemoteSocket),
            gen_tcp:close(Client),
            exit(communicateerror)
    end,

    transfer(Client, RemoteSocket).





transfer(Client, RemoteSocket) ->
    receive
        {tcp, Client, Request} ->
            case gen_tcp:send(RemoteSocket, transform:transform(Request)) of
                ok ->
                    transfer(Client, RemoteSocket);
                {error, _Error} ->
                    ok
            end;
        {tcp, RemoteSocket, Response} ->
            case gen_tcp:send(Client, transform:transform(Response)) of
                ok ->
                    transfer(Client, RemoteSocket);
                {error, _Error} ->
                    ok
            end;
        {tcp_closed, Client} ->
            ok;
        {tcp_error, Client, _Reason} ->
            io:format("client error~n", []),
            ok;
        {tcp_closed, RemoteSocket} ->
            ok;
        {tcp_error, RemoteSocket, _Reason} ->
            io:format("remote error~n", []),
            ok
    end,

    gen_tcp:close(RemoteSocket),
    gen_tcp:close(Client),
    ok.






find_target(Client) ->
    {ok, <<16#05:8, Nmethods:8>>} = gen_tcp:recv(Client, 2),
    {ok, _Methods} = gen_tcp:recv(Client, Nmethods),

    gen_tcp:send(Client, <<16#05, 16#0>>),
    {ok, <<16#05:8, 16#01:8, _Rsv:8, AType:8>>} = gen_tcp:recv(Client, 4),

    case AType of
        ?IPV4 ->
            {ok, <<Address:32>>} = gen_tcp:recv(Client, 4),
            {ok, <<Port:16>>} = gen_tcp:recv(Client, 2),
            <<?IPV4, Port:16, Address:32>>;
        ?IPV6 ->
            {ok, <<Address:128>>} = gen_tcp:recv(Client, 16),
            {ok, <<Port:16>>} = gen_tcp:recv(Client, 2),
            <<?IPV6, Port:16, Address:128>>;
        ?DOMAIN ->
            {ok, <<DomainLen:8>>} = gen_tcp:recv(Client, 1),
            {ok, <<DomainBin/binary>>} = gen_tcp:recv(Client, DomainLen),
            {ok, <<Port:16>>} = gen_tcp:recv(Client, 2),
            <<?DOMAIN, Port:16, DomainLen:8, DomainBin/binary>>
    end.


