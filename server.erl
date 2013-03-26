-module(server).

-export([start/0]).

-export([start_process/1]).



start() ->
	{_, _, _, Port} = utils:get_config(),
	Options = utils:options({0,0,0,0}),

	{ok, Socket} = gen_tcp:listen(Port, Options),
	io:format("Server listen on ~p~n", [Port]),

	accept(Socket).


accept(Socket) ->
	{ok, Client} = gen_tcp:accept(Socket),
	spawn(?MODULE, start_process, [Client]),
	accept(Socket).


start_process(Client) ->
	io:format("~nstart process: ~p~n", [Client]),

	{ok, <<Type:8, Port:16, Destination/binary>>} = gen_tcp:recv(Client, 0),

	Address = 
	case Type of
		1 ->
			list_to_tuple( binary_to_list(Destination) );
		3 ->
			binary_to_list(Destination)
	end,


	Options = utils:options(),

	io:format("Address: ~p, Port: ~p~n", [Address, Port]),

	case gen_tcp:connect(Address, Port, Options, 5000) of
		{ok, TargetSocket} ->
			% io:format("connect done...~n", []),
			gen_tcp:send(Client, <<0>>),
			case gen_tcp:recv(Client, 0) of
				{ok, Request} ->
					gen_tcp:send(TargetSocket, Request),
					transfer(Client, TargetSocket);
				{error, Error} ->
					io:format("get Request error: ~p~n", [Error]),
					gen_tcp:close(TargetSocket),
					gen_tcp:close(Client)
			end;
		{error, Error} ->
			io:format("connect error: ~p~n", [Error]),
			gen_tcp:close(Client)
	end,
	io:format("process die!~n", []).




transfer(Client, Remote) ->
	case gen_tcp:recv(Remote, 0) of 
		{ok, Data} ->
			ok = gen_tcp:send(Client, Data),
			transfer(Client, Remote);
		{error, Error} ->
			io:format("remote recv error: ~p~n", [Error]),
			gen_tcp:close(Client),
			gen_tcp:close(Remote)
	end.
