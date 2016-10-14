%%%-------------------------------------------------------------------
%%% @author wang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Oct 2016 下午5:26
%%%-------------------------------------------------------------------
-module(make_proxy).
-author("wang").

%% API
-export([start_server/0,
    start_client/0]).

start_server() ->
    {ok, _} = application:ensure_all_started(make_proxy),
    {ok, Port} = application:get_env(make_proxy, server_port),

    TransOpts = transport_opts(Port),

    {ok, _} = ranch:start_listener(
        make_proxy_server,
        20,
        ranch_tcp,
        TransOpts,
        mp_server_worker, []
    ).

start_client() ->
    {ok, _} = application:ensure_all_started(make_proxy),
    {ok, Port} = application:get_env(make_proxy, client_port),

    TransOpts = transport_opts(Port),

    {ok, _} = ranch:start_listener(
        make_proxy_client,
        20,
        ranch_tcp,
        TransOpts,
        mp_client_worker, []
    ).

transport_opts(Port) ->
    [{port, Port}, {max_connections, infinity}].