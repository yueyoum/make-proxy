-module(mp_crypto).
-export([encrypt/2,
         decrypt/2]).

-define(DATALENGTH, 16).
-define(IV, <<"^de$@#56*sxdfrtg">>).

-spec encrypt(nonempty_string(), binary()) -> binary().
encrypt(Key, Binary) ->
    BinaryLength = byte_size(Binary),
    Rem = (BinaryLength + 4) rem ?DATALENGTH,
    AdditionalLength = ?DATALENGTH - Rem,

    FinalBinary = <<BinaryLength:32/integer-big, Binary/binary, 0:AdditionalLength/unit:8>>,

    crypto:block_encrypt(aes_cbc128, Key, ?IV, FinalBinary).


-spec decrypt(nonempty_string(), binary()) -> {ok, binary()} |
                                               {error, term()}.
decrypt(Key, Binary) ->
    Data = crypto:block_decrypt(aes_cbc128, Key, ?IV, Binary),
    try
        <<Length:32/integer-big, RealData:Length/binary, _Rest/binary>> = Data,
        {ok, RealData}
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end.

