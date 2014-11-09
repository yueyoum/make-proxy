-module(mp_crypto).
-export([encrypt/2,
         decrypt/2]).

-define(DATALENGTH, 16).
-define(IV, <<"90de3456asxdfrtg">>).

encrypt(Key, Binary) ->
    BinaryLength = byte_size(Binary),
    Rem = (BinaryLength + 4) rem ?DATALENGTH,
    AdditionalLength = ?DATALENGTH - Rem,

    FinalBinary = <<BinaryLength:32/integer-big, Binary/binary, 0:AdditionalLength/unit:8>>,

    crypto:block_encrypt(aes_cbc128, Key, ?IV, FinalBinary).


decrypt(Key, Binary) ->
    Data = crypto:block_decrypt(aes_cbc128, Key, ?IV, Binary),
    try
        <<Length:32/integer-big, RealData:Length/binary, _Rest/binary>> = Data,
        {ok, RealData}
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end.

