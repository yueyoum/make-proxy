%%%-------------------------------------------------------------------
%%% @author wang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Oct 2016 下午2:32
%%%-------------------------------------------------------------------
-module(mp_client_protocol).
-author("wang").

-include("mp_client.hrl").

-callback detect_head(H :: byte()) -> boolean().
-callback request(Data :: binary(), State :: #client{}) ->
    {ok, State :: #client{}} |
    {error, Reason :: term()}.
