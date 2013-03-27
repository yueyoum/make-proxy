-module(server).

-export([start/0]).

-export([start_process/0,
        start_process/1,
        accept/1,
        start_server/0]).


-include("utils.hrl").
-include("config.hrl").

-define(WORKER_NUMS, 20).
-define(WORKER_TIMEOUT, 300000).


start() ->
    {ok, Socket} = gen_tcp:listen(?REMOTEPORT, ?OPTIONS({0,0,0,0})),
    io:format("Server listen on ~p~n", [?REMOTEPORT]),
    register(server, spawn(?MODULE, start_server, [])),
    accept(Socket).


accept(Socket) ->
    {ok, Client} = gen_tcp:accept(Socket),
    server ! {connect, Client},
    accept(Socket).



start_server() ->
    start_server(start_works(?WORKER_NUMS)).

%% main loop, accept new connections, reuse works, and purge dead works.
start_server(Works) ->
    NewWorks =
    receive
        {connect, ClientSock} ->
            manage_works(choose, Works, ClientSock);
        {'DOWN', _Ref, process, Pid, timeout} ->
            manage_works(timeout, Works, Pid);
        {reuse, Pid} ->
            manage_works(reuse, Works, Pid)
    end,
    start_server(NewWorks).


%% spawn some works as works pool.
start_works(Num) ->
    start_works(Num, []).

start_works(0, Works) ->
    Works;
start_works(Num, Works) ->
    {Pid, _Ref} = spawn_monitor(?MODULE, start_process, []),
    start_works(Num-1, [Pid | Works]).



manage_works(choose, [], ClientSock) ->
    %% no available for chosen, make new one
    {Pid, _Ref} = spawn_monitor(?MODULE, start_process, []),
    Pid ! {connect, ClientSock},
    start_works(?WORKER_NUMS);

manage_works(choose, [Pid | Tail], ClientSock) ->
    Pid ! {connect, ClientSock},
    Tail;

manage_works(timeout, Works, Pid) ->
    io:format("Clear timeout pid: ~p~n", [Pid]),
    lists:delete(Pid, Works);

manage_works(reuse, Works, Pid) ->
    io:format("Reuse Pid, back to pool: ~p~n", [Pid]),
    Works ++ [Pid].
    


start_process() ->
    receive
        {connect, Client} -> 
            start_process(Client),
            server ! {reuse, self()},
            start_process()
    after ?WORKER_TIMEOUT ->
        exit(timeout)
    end.





start_process(Client) ->
    case gen_tcp:recv(Client, 2) of
        {ok, <<TargetLen:16>>} ->
            parse_target(TargetLen, Client);
        {error, _Error} ->
            gen_tcp:close(Client)
    end.


parse_target(TargetLen, Client) ->
    {ok, <<Type:8, Port:16, Destination/binary>>} = gen_tcp:recv(Client, TargetLen),
    {ok, <<Request/binary>>} = gen_tcp:recv(Client, 0),


    Address = 
    case Type of
        ?IPV4 ->
            list_to_tuple( binary_to_list(Destination) );
        ?DOMAIN ->
            binary_to_list(Destination)
    end,

    io:format("Address: ~p, Port: ~p~n", [Address, Port]),

    case connect_target(Address, Port, 3) of
        {ok, TargetSocket} ->
            ok = gen_tcp:send(TargetSocket, Request),
            transfer(Client, TargetSocket);
        error ->
            gen_tcp:close(Client)
    end.



connect_target(_, _, 0) ->
    error;
connect_target(Address, Port, Times) ->
    case gen_tcp:connect(Address, Port, ?OPTIONS, ?TIMEOUT) of
        {ok, TargetSocket} ->
            {ok, TargetSocket};
        {error, _Error} ->
            connect_target(Address, Port, Times-1)
    end.



transfer(Client, Remote) ->
    case gen_tcp:recv(Remote, 0, ?TIMEOUT) of 
        {ok, Data} ->
            ok = gen_tcp:send(Client, Data),
            transfer(Client, Remote);
        {error, _Error} ->
            gen_tcp:close(Client),
            gen_tcp:close(Remote)
    end.
