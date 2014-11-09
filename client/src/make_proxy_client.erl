-module(make_proxy_client).
-export([start/0]).

start() ->
    application:start(make_proxy_client).

