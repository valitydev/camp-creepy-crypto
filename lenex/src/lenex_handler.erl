-module(lenex_handler).

-export([init/2]).

%%

-spec init(cowboy_req:req(), Opts) ->
    {ok, cowboy_req:req(), Opts}.

init(Req0, Opts) ->
    Req1 = init(cowboy_req:method(Req0), Req0, Opts),
    {ok, Req1, Opts}.

init(<<"GET">>, Req, Opts) ->
    QsVals = maps:from_list(cowboy_req:parse_qs(Req)),
    case maps:find(<<"user">>, QsVals) of
        {ok, Username} ->
            handle_get_status(Username, Req, Opts);
        error ->
            cowboy_req:reply(400, Req)
    end;

init(<<"POST">>, Req0, Opts) ->
    case cowboy_req:read_body(Req0, #{length => 64000, period => 30000}) of
        {ok, Body, Req1} ->
            handle_post_status(maps:from_list(cow_qs:parse_qs(Body)), Body, Req1, Opts);
        _ ->
            cowboy_req:reply(413, Req0)
    end;

init(_, Req, _Opts) ->
    cowboy_req:reply(405, Req).

%%

handle_get_status(Username, Req, #{db := DB}) ->
    case read_db({Username, status}, DB) of
        {ok, Status} ->
            cowboy_req:reply(
                200,
                #{<<"content-type">> => <<"text/plain">>},
                Status,
                Req
            );
        {error, notfound} ->
            cowboy_req:reply(204, Req)
    end.

%%

handle_post_status(QsVals, Body, Req0, Opts) ->
    Username = maps:get(<<"user">>, QsVals, undefined),
    Status = maps:get(<<"v">>, QsVals, undefined),
    Digest = cowboy_req:header(<<"signature">>, Req0),
    Sign = try genlib_format:hex_to_binary(Digest) catch
        error:badarg ->
            undefined
    end,
    case lists:all(fun (V) -> V /= undefined end, [Username, Status, Sign]) of
        true ->
            case verify_signature(Body, Sign, Opts) of
                true ->
                    post_status(Username, Status, Req0, Opts);
                false ->
                    cowboy_req:reply(403, Req0)
            end;
        false ->
            cowboy_req:reply(400, Req0)
    end.

verify_signature(Body, Sign, #{key := Key}) ->
    lenex_crypto:verify(Body, Sign, Key).

post_status(Username, Status, Req0, #{db := DB}) ->
    ok = write_db({Username, status}, Status, DB),
    cowboy_req:reply(200, Req0).

%%

read_db(Key, DB) ->
    case ets:lookup(DB, Key) of
        [{Key, Value}] ->
            {ok, Value};
        [] ->
            {error, notfound}
    end.

write_db(Key, Value, DB) ->
    true = ets:insert(DB, [{Key, Value}]),
    ok.
