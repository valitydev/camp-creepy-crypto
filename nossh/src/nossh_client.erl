-module(nossh_client).

-export([new/2]).

%%

-include("ssh.hrl").

new(Secret, SID) ->
    nossh_ssh_transport:alg_init(snd, #ssh{
        role                  = client,
        algorithms            = #alg{kex = 'rsa-sha2-256'},
        recv_mac              = 'hmac-sha2-256',
        send_mac              = 'hmac-sha2-256',
        encrypt               = 'aes128-cbc',
        decrypt               = 'aes128-cbc',
        decompress            = none,
        random_length_padding = 0,
        shared_secret         = ssh_bits:mpint(Secret),
        exchanged_hash        = crypto:hash(sha256, <<"Woopee">>),
        session_id            = SID
    }).
