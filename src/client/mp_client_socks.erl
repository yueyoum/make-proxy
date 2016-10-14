-module(mp_client_socks).

-behaviour(mp_client_protocol).

-export([detect_head/1,
    request/2]).

-include("mp_client.hrl").

detect_head(4) ->
    true;

detect_head(5) ->
    true;

detect_head(_) ->
    false.

request(Data,
    #client{key = Key, socket = Socket, transport = Transport,
        remote = undefined, buffer = Buffer} = State) ->

    Data1 = <<Buffer/binary, Data/binary>>,
    case find_target(Data1) of
        {ok, Target, Body, Response} ->
            case mp_client_utils:connect_to_remote() of
                {ok, Remote} ->
                    EncryptedTarget = mp_crypto:encrypt(Key, term_to_binary(Target)),
                    ok = gen_tcp:send(Remote, EncryptedTarget),
                    case Body of
                        <<>> -> ok;
                        _ ->
                            ok = gen_tcp:send(Remote, mp_crypto:encrypt(Key, Body))
                    end,
                    ok = Transport:send(Socket, Response),
                    {ok, State#client{remote = Remote}};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason};
        more ->
            case byte_size(Buffer) =:= 0 of
                true ->
                    % reply success info
                    ok = Transport:send(Socket, <<5, 0>>);
                false ->
                    ok
            end,
            {ok, State#client{buffer = Data1}}
    end;

request(Data, #client{key = Key, remote = Remote} = State) ->
    ok = gen_tcp:send(Remote, mp_crypto:encrypt(Key, Data)),
    {ok, State}.


%% http://www.openssh.com/txt/socks4.protocol
-spec find_target(binary()) ->
    {ok, mp_target(), binary(), binary()} |
    {error, term()} |
    more.
find_target(<<4:8, _CD:8, Port:16, Address:4/binary, Rest/binary>>) ->
    case split_socks4_data(Rest) of
        {ok, _UserID, Body} ->
            Target = {list_to_tuple(binary_to_list(Address)), Port},
            Response = <<0:8, 90:8, Port:16, Address:4/binary>>,
            {ok, Target, Body, Response};
        {error, Reason} ->
            {error, Reason};
        more ->
            more
    end;

find_target(<<4:8, _Rest/binary>>) ->
    more;

%% https://www.ietf.org/rfc/rfc1928.txt
find_target(<<5:8, N:8, _Methods:N/binary-unit:8,
    5:8, _CMD:8, _Rsv:8, AType:8, Rest/binary>>) ->

    case split_socks5_data(AType, Rest) of
        {ok, Target, Body} ->
            Response = <<5, 0, 0, 1, <<0, 0, 0, 0>>/binary, 0:16>>,
            {ok, Target, Body, Response};
        {error, Reason} ->
            {error, Reason};
        more ->
            more
    end;

find_target(<<5:8, _Rest/binary>>) ->
    more;

find_target(_) ->
    {error, invalid_data}.


-spec split_socks5_data(integer(), binary()) ->
    {ok, mp_target(), binary()} |
    {error, term()} |
    more.
split_socks5_data(1, <<Address:4/binary, Port:16, Body/binary>>) ->
    Target = {list_to_tuple(binary_to_list(Address)), Port},
    {ok, Target, Body};

split_socks5_data(1, _) ->
    more;

split_socks5_data(3, <<Len:8, Domain:Len/binary, Port:16, Body/binary>>) ->
    Target = {binary_to_list(Domain), Port},
    {ok, Target, Body};

split_socks5_data(3, _) ->
    more;

split_socks5_data(4, <<Address:16/binary, Port:16, Body/binary>>) ->
    Target = {list_to_tuple(binary_to_list(Address)), Port},
    {ok, Target, Body};

split_socks5_data(4, _) ->
    more;

split_socks5_data(_, _) ->
    {error, invalid_data}.

-spec split_socks4_data(binary()) ->
    {ok, string(), binary()} |
    {error, term()} |
    more.
split_socks4_data(Data) ->
    split_socks4_data(Data, []).

split_socks4_data(<<>>, _UserID) ->
    more;

split_socks4_data(<<0:8, Body/binary>>, UserID) ->
    ID = lists:flatten(lists:join("", lists:reverse(UserID))),
    {ok, ID, Body};

split_socks4_data(<<Part:8, Rest/binary>>, UserID) ->
    case length(UserID) > 1024 of
        true ->
            {error, userid_too_long};
        false ->
            split_socks4_data(Rest, [integer_to_list(Part) | UserID])
    end.
