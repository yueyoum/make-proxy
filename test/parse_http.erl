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

parse_1_test() ->
    Data = <<"GET http://www.baidu.com HTTP/1.1\r\n\r\n">>,
    {_, Req} = mp_client_http:parse_http_request(Data),
    ?assertEqual(Req#http_request.status, done),
    ?assertEqual(Req#http_request.host, "www.baidu.com"),
    ?assertEqual(Req#http_request.port, 80).

parse_2_test() ->
    Data = <<"GET https://google.com HTTP/1.1\r\nHost: google.com\r\n\r\nGET https://youbute.com/ HTTP">>,
    {_, Req} = mp_client_http:parse_http_request(Data),
    ?assertEqual(Req#http_request.status, done),
    ?assertEqual(Req#http_request.host, "google.com"),
    ?assertEqual(Req#http_request.port, 443),
    ?assertEqual(Req#http_request.next_data, <<"GET https://youbute.com/ HTTP">>).

parse_3_test() ->
    Data = <<"POST https://google.com/ HTTP/1.1\r\nContent-Length: 5\r\n\r\nHelloGET https://youbute.com/ HTTP">>,
    {_, Req} = mp_client_http:parse_http_request(Data),
    ?assertEqual(Req#http_request.status, done),
    ?assertEqual(Req#http_request.next_data, <<"GET https://youbute.com/ HTTP">>).

parse_4_test() ->
    Data = <<"CONNECT xyz.com:4430/path/ HTTP/1.1\r\n\r\n">>,
    {_, Req} = mp_client_http:parse_http_request(Data),
    ?assertEqual(Req#http_request.status, done),
    ?assertEqual(Req#http_request.method, <<"CONNECT">>),
    ?assertEqual(Req#http_request.host, "xyz.com"),
    ?assertEqual(Req#http_request.port, 4430).