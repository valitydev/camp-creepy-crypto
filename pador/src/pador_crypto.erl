-module(pador_crypto).

-export([encrypt/3]).
-export([decrypt/2]).

-type iv()         :: <<_:128>>.
-type plaintext()  :: binary().
-type ciphertext() :: <<_:128, _:_*128>>.
-type key()        :: <<_:128>>.

%%

-spec encrypt(plaintext(), iv(), key()) ->
    ciphertext().

encrypt(PT, IV = <<_:16/binary>>, K = <<_:16/binary>>) ->
    CT0 = crypto:block_encrypt(aes_cbc, K, IV, pad(PT)),
    CT1 = <<IV:16/binary, CT0/binary>>,
    CT1;
encrypt(_, _, _) ->
    error(badarg).

pad(PT) ->
    pad(16 - (byte_size(PT) rem 16), PT).

pad(PadSize, PT) ->
    Padding = gen_padding(PadSize),
    <<PT/binary, Padding/binary>>.

%%

-spec decrypt(ciphertext(), key()) ->
    plaintext().

decrypt(<<IV:16/binary, CT/binary>>, K = <<_:16/binary>>) when byte_size(CT) rem 16 == 0 ->
    PT0 = crypto:block_decrypt(aes_cbc, K, IV, CT),
    PT1 = unpad(PT0),
    PT1;
decrypt(_, _) ->
    error(badarg).

unpad(PT) ->
    PadSize = binary:last(PT),
    unpad(gen_padding(PadSize + 1), PT).

unpad(Padding, PT0) ->
    PadSize = byte_size(Padding),
    PTSize = byte_size(PT0) - PadSize,
    <<PT:PTSize/binary, Padding:PadSize/binary>> = PT0,
    PT.


%%

gen_padding(PadSize) when PadSize > 0, PadSize < 16 ->
    << <<(PadSize - 1):8>> || _ <- lists:seq(1, PadSize)>>;
gen_padding(PadSize) when PadSize >= 16 ->
    << <<16:8>> || _ <- lists:seq(1, 16)>>.
