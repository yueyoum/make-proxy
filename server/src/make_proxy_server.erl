-module(make_proxy_server).
-export([start/0]).

start() ->
    application:start(make_proxy_server).

