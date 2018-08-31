-module(bleach_crypto).

-export([decrypt/2]).

-type ct() :: binary().
-type pt() :: binary().
-type sk() :: public_key:rsa_private_key().

%%

-include_lib("public_key/include/public_key.hrl").

-spec decrypt(ct(), sk()) ->
    pt().

decrypt(Ct, Sk) ->
    Pt = public_key:decrypt_private(Ct, Sk, [{rsa_pad, rsa_no_padding}]),
    interpret(
        binary:decode_unsigned(Pt),
        Sk#'RSAPrivateKey'.modulus
    ).

interpret(Pt, N) when N div 2 > Pt ->
    Pt.
