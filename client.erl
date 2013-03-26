-module(client).

-export([start/0]).

-export([
    start_process/1,
    accept/1
]).

-include("utils.erl").




start() ->
    {LocalIP, LocalPort, _, _} = utils:get_config(),
    Options = utils:options(LocalIP),

    {ok, Socket} = gen_tcp:listen(LocalPort, Options),
    io:format("Porxy listen on ~p:~p~n", [LocalIP, LocalPort]),
    accept(Socket).
    % lists:foreach(
    %     fun(_) ->
    %         spawn(?MODULE, accept, [Socket])
    %     end,
    %     [1,1,1,1]
    % ),
    % ok.


accept(Socket) ->
    {ok, Client} = gen_tcp:accept(Socket),
    spawn(?MODULE, start_process, [Client]),
    accept(Socket).



start_process(Client) ->
    io:format("~nstart process: ~p~n", [Client]),

    {_LocalIP, _LocalPort, RemoteIP, RemotePort} = utils:get_config(),

    case gen_tcp:connect(RemoteIP, RemotePort, utils:options()) of
        {ok, RemoteSocket} ->
            go_on(Client, RemoteSocket);
        {error, Error} ->
            io:format("Connect error, ~p. ~p:~p~n", [Error, RemoteIP, RemotePort]),
            gen_tcp:close(Client),
            exit("connect error")
    end.



go_on(Client, RemoteSocket) ->
    {LocalIP, LocalPort, _, _} = get_config(),

    Target = find_target(Client),
    ok = gen_tcp:send(RemoteSocket, Target),

    IP = list_to_binary(tuple_to_list(LocalIP)),
    BackData = <<5, 0, 0, 1, IP/binary, LocalPort:16>>,
    ok = gen_tcp:send(Client, BackData),



    case gen_tcp:recv(RemoteSocket, 1) of
        {ok, <<0>>} ->
            case gen_tcp:recv(Client, 0) of
                {ok, Request} ->
                    ok = gen_tcp:send(RemoteSocket, Request),
                    transfer(Client, RemoteSocket);
                {error, Error} ->
                    io:format("get Request error: ~p~n", [Error]),
                    gen_tcp:close(RemoteSocket),
                    gen_tcp:close(Client)
            end;
        _ ->
            io:format("remote not connected ~n", []),
            gen_tcp:close(RemoteSocket),
            gen_tcp:close(Client)
    end,

    io:format("process die!~n", []).





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
        {error, Error} ->
            io:format("error... ~p~n", [Error]),
            gen_tcp:close(Client),
            gen_tcp:close(RemoteSocket)
    end.



