-module(epush_websvr).

-export([init/2]).

-include_lib("eutil/include/eutil.hrl").


init(Req0, Opts) ->
    {ok, OriPostVals, _Req} = cowboy_req:read_body(Req0),
    Payload = eutil:json_decode(OriPostVals),
    {Code, Body} = do_handle(Payload),
    Req = cowboy_req:reply(Code, #{<<"content-type">> => <<"application/json">>}, Body, Req0),
    {ok, Req, Opts}.

do_handle(Payload) ->
    #{<<"epush_id">> := EpushId} = Payload,
    case epush_util:get_push_conf(EpushId) of
        undefined ->
            {404, <<"No this EpushId Config">>};
        Conf ->
            Type = maps:get(type, Conf),
            Mod = erlang:list_to_existing_atom(lists:concat([epush_, Type])),
            %%todo: 返回的要整理
            case Mod:handle_http(Conf, Payload) of
                ok -> {200, <<"">>};
                {ok, Result} -> {200, eutil:json_encode(Result)};
                {_, Error} -> {400, eutil:json_encode(Error)}

            end
    end.
