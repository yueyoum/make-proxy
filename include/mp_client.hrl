%%%-------------------------------------------------------------------
%%% @author wang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Oct 2016 下午7:46
%%%-------------------------------------------------------------------
-author("wang").

-record(client, {
    key :: string(),
    ref :: ranch:ref(),
    socket :: any(),
    transport :: module(),
    ok,
    closed,
    error,
    remote :: gen_tcp:socket() | undefined,
    protocol :: module() | undefined,
    buffer :: binary()
}).

-type mp_target() :: {inet:ip_address() | nonempty_string(), inet:port_number()}.
