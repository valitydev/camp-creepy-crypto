-module(bleach_handler).

-export([init/2]).

%%

-spec init(cowboy_req:req(), Opts) ->
    {ok, cowboy_req:req(), Opts}.

init(Req0, Opts) ->
    Req = case cowboy_req:method(Req0) of
        <<"POST">> ->
            handle_body(Req0, Opts);
        _ ->
            cowboy_req:reply(405, Req0)
    end,
    {ok, Req, Opts}.

handle_body(Req0, Opts) ->
    case cowboy_req:read_urlencoded_body(Req0) of
        {ok, Vs, Req} ->
            handle_ciphertext(proplists:get_value(<<"ct">>, Vs), Req, Opts);
        _ ->
            cowboy_req:reply(400, Req0)
    end.

handle_ciphertext(CT0, Req0, Opts) when is_binary(CT0) ->
    try
        CT1 = genlib_format:hex_to_binary(CT0),
        _V = bleach_crypto:decrypt(CT1, maps:get(sk, Opts)),
        cowboy_req:reply(204, Req0)
    catch error:badarg ->
        cowboy_req:reply(400, Req0)
    end;

handle_ciphertext(undefined, Req0, _Opts) ->
    cowboy_req:reply(400, Req0).
