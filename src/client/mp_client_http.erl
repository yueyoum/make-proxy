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

request(Data, #client{key = Key, remote = undefined, buffer = Buffer} = State) ->
    Data1 = <<Buffer/binary, Data/binary>>,
    Req = parse_http_request(Data1),

    case Req#http_request.status of
        done ->
            #http_request{host = Host, port = Port, next_data = NextData} = Req,
            Target = {Host, Port},

            case mp_client_utils:connect_to_remote() of
                {ok, Remote} ->
                    EncryptedTarget = mp_crypto:encrypt(Key, term_to_binary(Target)),
                    ok = gen_tcp:send(Remote, EncryptedTarget),

                    XX = binary:part(Data1, 0, byte_size(Data1) - byte_size(NextData)),
                    EncryptedData = mp_crypto:encrypt(Key, XX),
                    ok = gen_tcp:send(Remote, EncryptedData),
                    {ok, State#client{remote = Remote, buffer = NextData}};
                {error, Reason} ->
                    {error, Reason}
            end;
        error ->
            {error, parse_http_request_error};
        more ->
            {ok, State#client{buffer = Data1}}
    end;

request(Data, #client{key = _Key, remote = _Remote} = State) ->
    gen_tcp:close(_Remote),
    request(Data, State#client{remote = undefined}).


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

do_parse({ok, {http_error, _}, Rest}, Req) ->
    do_parse(erlang:decode_packet(httph_bin, Rest, []), Req);

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
    {Host, Port} =
    case binary:split(Value, <<":">>) of
        [Value] ->
            {binary_to_list(Value), 80};
        [DomainBin, PortBin] ->
            {binary_to_list(DomainBin), binary_to_integer(PortBin)}
    end,

    Req1 = Req#http_request{host = Host, port = Port},
    do_parse(erlang:decode_packet(httph_bin, Rest, []), Req1);

do_parse({ok, {http_header, _Num, 'Content-Length', _, Value}, Rest}, Req) ->
    Req1 = Req#http_request{content_length = binary_to_integer(Value)},
    do_parse(erlang:decode_packet(httph_bin, Rest, []), Req1);

do_parse({ok, {http_header, _, _, _, _}, Rest}, Req) ->
    do_parse(erlang:decode_packet(httph_bin, Rest, []), Req).
