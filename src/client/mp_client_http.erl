-module(mp_client_http).

-behaviour(mp_client_protocol).

-export([detect_head/1,
    request/2,
    parse_http_request/1]).

-include("mp_client.hrl").
-include("mp_http_request.hrl").

-define(HTTP_METHOD_HEAD, [
    $G,   % GET
    $P,   % POST PUT
    $H,   % HEAD
    $D,   % DELETE
    $T,   % TRACE
    $C,   % CONNECT
    $O    % OPTIONS
]).

detect_head(H) ->
    lists:member(H, ?HTTP_METHOD_HEAD).

request(Data, #client{remote = undefined, buffer = Buffer} = State) ->
    Data1 = <<Buffer/binary, Data/binary>>,
    {Data2, Req} = parse_http_request(Data1),

    case Req#http_request.status of
        done ->
            do_communication(Data2, Req, State);
        error ->
            {error, parse_http_request_error};
        more ->
            {ok, State#client{buffer = Data1}}
    end;

request(Data, #client{remote = Remote, keep_alive = false} = State) ->
    gen_tcp:close(Remote),
    request(Data, State#client{remote = undefined});

request(Data, #client{key = Key, remote = Remote, keep_alive = true} = State) ->
    ok = gen_tcp:send(Remote, mp_crypto:encrypt(Key, Data)),
    {ok, State}.

-spec do_communication(binary(), #http_request{}, #client{}) ->
    {ok, #client{}} |
    {error, term()}.
do_communication(Data,
    #http_request{host = Host, port = Port, next_data = NextData} = Req,
    #client{key = Key, socket = Socket, transport = Transport} = State) ->

    case mp_client_utils:connect_to_remote() of
        {ok, Remote} ->
            ok = gen_tcp:send(Remote, mp_crypto:encrypt(Key, term_to_binary({Host, Port}))),

            State1 = State#client{remote = Remote, buffer = NextData},

            case Req#http_request.method =:= <<"CONNECT">> of
                true ->
                    Transport:send(Socket, <<"HTTP/1.1 200 OK\r\n\r\n">>),
                    {ok, State1#client{keep_alive = true}};
                false ->
                    ThisData = binary:part(Data, 0, byte_size(Data) - byte_size(NextData)),
                    ok = gen_tcp:send(Remote, mp_crypto:encrypt(Key, ThisData)),
                    {ok, State1}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec parse_http_request(binary()) -> {binary(), #http_request{}}.
parse_http_request(Data) ->
    parse_http_request(Data, #http_request{}).

-spec parse_http_request(binary(), #http_request{}) -> {binary(), #http_request{}}.
parse_http_request(Data, Req) ->
    case binary:split(Data, <<"\r\n">>) of
        [Data] ->
            Req1 = Req#http_request{status = more},
            {Data, Req1};
        [RequestLine, Headers] ->
            {RequestLine1, Req1} = parse_request_line(RequestLine, Req),
            Req2 =
            case Req1#http_request.method of
                <<"CONNECT">> ->
                    Req1#http_request{status = done};
                _ ->
                    do_parse(erlang:decode_packet(httph_bin, Headers, []), Req1)
            end,

            Data1 = erlang:iolist_to_binary([RequestLine1, "\r\n", Headers]),
            {Data1, Req2}
    end.

-spec do_parse(tuple(), #http_request{}) -> #http_request{}.
do_parse({error, _}, Req) ->
    Req#http_request{status = error};

do_parse({more, _}, Req) ->
    Req#http_request{status = more};

do_parse({ok, http_eoh, Rest}, #http_request{content_length = 0} = Req) ->
    Req#http_request{
        status = done,
        next_data = Rest
    };

do_parse({ok, http_eoh, Rest},
    #http_request{content_length = ContentLength,
        current_length = CurrentLength} = Req) when ContentLength =:= CurrentLength ->
    Req#http_request{
        status = done,
        next_data = Rest
    };

do_parse({ok, http_eoh, Rest},
    #http_request{content_length = ContentLength, current_length = CurrentLength} = Req) ->

    MoreNeedLength = ContentLength - CurrentLength,
    RestLen = byte_size(Rest),

    case RestLen >= MoreNeedLength of
        true ->
            <<_Body:MoreNeedLength/binary, NextData/binary>> = Rest,
            Req#http_request{
                status = done,
                current_length = CurrentLength + MoreNeedLength,
                next_data = NextData
            };
        false ->
            Req#http_request{
                status = more,
                current_length = CurrentLength + RestLen
            }
    end;

%%do_parse({ok, {http_header, _Num, 'Host', _, Value}, Rest}, Req) ->
%%    {Host, Port} = split_host_and_port(Value),
%%
%%    Req1 = Req#http_request{host = Host, port = Port},
%%    do_parse(erlang:decode_packet(httph_bin, Rest, []), Req1);

do_parse({ok, {http_header, _Num, 'Content-Length', _, Value}, Rest}, Req) ->
    Req1 = Req#http_request{content_length = binary_to_integer(Value)},
    do_parse(erlang:decode_packet(httph_bin, Rest, []), Req1);

do_parse({ok, {http_header, _, _, _, _}, Rest}, Req) ->
    do_parse(erlang:decode_packet(httph_bin, Rest, []), Req).

-spec parse_request_line(binary(), #http_request{}) -> {binary(), #http_request{}}.
parse_request_line(RequestLine, Req) ->
    [Method, URL, Version] = binary:split(RequestLine, <<" ">>, [global]),
    {ok, P} = re:compile("^((?<Ascheme>http|https)://)?(?<Bhost>[^:|^/]+):?(?<Cport>\\d*)(?<Dpath>/?.*)"),
    {match, [Scheme, Host, Port, Path]} = re:run(
        URL,
        P,
        [{capture, all_names, binary}]
    ),

    Port1 = find_port(Scheme, Port),
    Path1 =
        case Path of
            <<>> -> <<"/">>;
            _ -> Path
        end,

    RequestLine1 = erlang:iolist_to_binary([Method, " ", Path1, " ", Version]),

    Req1 = Req#http_request{
        method = Method,
        host = binary_to_list(Host),
        port = Port1
    },

    {RequestLine1, Req1}.

-spec find_port(binary(), binary()) -> integer().
find_port(<<"http">>, <<>>) ->
    80;

find_port(<<"https">>, <<>>) ->
    443;

find_port(_, PortBin) ->
    binary_to_integer(PortBin).