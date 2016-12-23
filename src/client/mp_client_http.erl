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
    Req = parse_http_request(Data1),

    case Req#http_request.status of
        done ->
            do_communication(Data1, Req, State);
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

-spec parse_http_request(binary()) -> #http_request{}.
parse_http_request(Data) ->
    parse_http_request(Data, #http_request{}).

-spec parse_http_request(binary(), #http_request{}) -> #http_request{}.
parse_http_request(Data, Req) ->
    do_parse(erlang:decode_packet(httph_bin, Data, []), Req).

-spec do_parse(tuple(), #http_request{}) -> #http_request{}.
do_parse({error, _}, Req) ->
    Req#http_request{status = error};

do_parse({more, _}, Req) ->
    Req#http_request{status = more};

do_parse({ok, {http_error, Line}, Rest}, Req) ->
    [Method, Target, _Version] = binary:split(Line, <<" ">>, [global]),
    Req1 = Req#http_request{method = Method},

    case Method of
        <<"CONNECT">> ->
            {Host, Port} = split_host_and_port(Target),
            Req1#http_request{
                status = done,
                host = Host,
                port = Port
            };
        _ ->
            do_parse(erlang:decode_packet(httph_bin, Rest, []), Req)
    end;

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

do_parse({ok, {http_header, _Num, 'Host', _, Value}, Rest}, Req) ->
    {Host, Port} = split_host_and_port(Value),

    Req1 = Req#http_request{host = Host, port = Port},
    do_parse(erlang:decode_packet(httph_bin, Rest, []), Req1);

do_parse({ok, {http_header, _Num, 'Content-Length', _, Value}, Rest}, Req) ->
    Req1 = Req#http_request{content_length = binary_to_integer(Value)},
    do_parse(erlang:decode_packet(httph_bin, Rest, []), Req1);

do_parse({ok, {http_header, _, _, _, _}, Rest}, Req) ->
    do_parse(erlang:decode_packet(httph_bin, Rest, []), Req).


-spec split_host_and_port(binary()) -> {nonempty_string(), integer()}.
split_host_and_port(Target) ->
    case binary:split(Target, <<":">>) of
        [Target] ->
            {binary_to_list(Target), 80};
        [DomainBin, PortBin] ->
            {binary_to_list(DomainBin), binary_to_integer(PortBin)}
    end.
