%%%-------------------------------------------------------------------
%%% @author wang
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Oct 2016 上午11:51
%%%-------------------------------------------------------------------
-module(parse_http).
-author("wang").

-include_lib("eunit/include/eunit.hrl").

-include("mp_http_request.hrl").

parse_more_test() ->
    Data = <<"GET / HTTP/1.1\r\n">>,
    Req = mp_client_http:parse_http_request(Data),
    ?assertEqual(Req#http_request.status, more).

parse_done_test() ->
    Data = <<"Host: google.com\r\n\r\n">>,
    Req = mp_client_http:parse_http_request(Data),
    ?assertEqual(Req#http_request.status, done),
    ?assertEqual(Req#http_request.host, "google.com"),
    ?assertEqual(Req#http_request.port, 80).

parse_next_data_test() ->
    Data = <<"Host: google.com\r\n\r\nGET /path/ HTTP">>,
    Req = mp_client_http:parse_http_request(Data),
    ?assertEqual(Req#http_request.status, done),
    ?assertEqual(Req#http_request.next_data, <<"GET /path/ HTTP">>).

parse_body_test() ->
    Data = <<"POST / HTTP/1.1\r\nContent-Length: 5\r\n\r\nHelloGET /path/ HTTP">>,
    Req = mp_client_http:parse_http_request(Data),
    ?assertEqual(Req#http_request.status, done),
    ?assertEqual(Req#http_request.next_data, <<"GET /path/ HTTP">>).