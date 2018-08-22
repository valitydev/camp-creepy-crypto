-module(lenex_crypto).

-export([sign/2]).
-export([verify/3]).

%%

-type message()   :: binary().
-type key()       :: <<_:128>>.
-type signature() :: <<_:256>>.

-spec sign(message(), key()) ->
    signature().

-spec verify(message(), signature(), key()) ->
    boolean().

sign(M, K) ->
    crypto:hash(sha256, <<K/binary, M/binary>>).

verify(M, T, K) ->
    sign(M, K) =:= T.
