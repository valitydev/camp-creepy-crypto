-module(nossh_handler).

-export([start_link/4]).
-export([connection_process/5]).

%%

-include("ssh.hrl").

-spec start_link(ranch:ref(), inet:socket(), module(), _Opts) ->
    {ok, pid()}.

start_link(Ref, Socket, Transport, Opts) ->
    Pid = proc_lib:spawn_link(?MODULE, connection_process,
        [self(), Ref, Socket, Transport, Opts]),
    {ok, Pid}.

-spec connection_process(pid(), ranch:ref(), inet:socket(), module(), _Opts) ->
    ok.

connection_process(Parent, Ref, Socket, Transport, Opts) ->
    ok = ranch:accept_ack(Ref),
    Ssh = nossh_ssh_transport:alg_init(snd, nossh_ssh_transport:alg_init(rcv, #ssh{
        role                  = server,
        algorithms            = #alg{kex = 'rsa-sha2-256'},
        recv_mac              = 'hmac-sha2-256',
        send_mac              = 'hmac-sha2-256',
        encrypt               = 'aes128-cbc',
        decrypt               = 'aes128-cbc',
        decompress            = none,
        random_length_padding = 0,
        shared_secret         = ssh_bits:mpint(maps:get(ssh_shared_secret, Opts)),
        exchanged_hash        = crypto:hash(sha256, <<"Woopee">>),
        session_id            = maps:get(ssh_session_id, Opts)
    })),
    loop(#{
        parent    => Parent,
        ref       => Ref,
        socket    => Socket,
        transport => Transport,
        ssh       => Ssh,
        opts      => Opts
    }).

%%

loop(St = #{socket := Socket, transport := Transport, opts := Opts}) ->
    _ = io:format("~n", []),
    _ = io:format("===== State =====~n~p~n=====~n", [St]),
    _ = Transport:setopts(Socket, [{active, once}]),
    {OK, Closed, Error} = Transport:messages(),
    InactivityTimeout = maps:get(inactivity_timeout, Opts, 300000),
    receive
        {OK, Socket, Data} ->
            case handle_packet_part(Data, St) of
                {ok, Packet, St1} ->
                    _ = io:format("===== Packet from ~p =====~n", [Socket]),
                    _ = swab_hexdump:hexdump(Packet),
                    _ = io:format("~n", []),
                    loop(St1);
                {more, St1} ->
                    _ = io:format("===== More bytes needed =====~n", []),
                    loop(St1);
                {error, Reason} ->
                    _ = io:format("===== Protocol error =====~n~p~n=====~n", [Reason]),
                    exit({shutdown, {protocol_error, Reason}})
            end;
        {Closed, Socket} ->
            exit({shutdown, {socket_error, closed}});
        {Error, Socket} ->
            exit({shutdown, {socket_error, Error}})
    after
        InactivityTimeout ->
            exit({shutdown, {internal_error, timeout}})
    end.

handle_packet_part(Data, St = #{packet := {UndecryptedPktLen, Decrypted, Encrypted}, ssh := Ssh}) ->
    try nossh_ssh_transport:handle_packet_part(
        Decrypted,
        <<Encrypted/binary, Data/binary>>,
        UndecryptedPktLen,
        Ssh
    ) of
        {packet_decrypted, Decrypted1, EncryptedRest, Ssh1} ->
            {ok, Decrypted1, St#{
                packet := {undefined, <<>>, EncryptedRest},
                ssh := Ssh1#ssh{recv_sequence = ssh_transport:next_seqnum(Ssh1#ssh.recv_sequence)}
            }};
        {get_more, Decrypted1, EncryptedRest, RemainingPktLen, Ssh1} ->
            {more, St#{
                packet := {RemainingPktLen, Decrypted1, EncryptedRest},
                ssh := Ssh1
            }};
        {bad_mac, _Ssh1} ->
            {error, bad_mac};
        {error, Reason} ->
            {error, Reason}
    catch
        C:E ->
            {error, {exception, {C, E, erlang:get_stacktrace()}}}
    end;

handle_packet_part(Data, St = #{}) ->
    handle_packet_part(Data, St#{
        packet => {undefined, <<>>, <<>>}
    }).
