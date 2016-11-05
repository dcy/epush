-module(epush_websvr).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include_lib("eutil/include/eutil.hrl").

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    Payload = eutil:get_cowboy_post_vals(Req),
    {Code, Body} = do_handle(Payload),
    {ok, Req2} = cowboy_req:reply(Code,
                                  [{<<"content-type">>, <<"application/json">>},
                                   {<<"connection">>, <<"keep-alive">>}
                                  ],
                                  Body, Req),
    {ok, Req2, State}.

                       
terminate(_Reason, _Req, _State) ->
    ok.


do_handle(Payload) ->
    #{<<"epush_id">> := EpushId} = Payload,
    case get_push_conf(EpushId) of
        undefined ->
            {404, <<"No this EpushId Config">>};
        Conf ->
            Type = maps:get(type, Conf),
            Mod = erlang:list_to_atom(lists:concat([epush_, Type])),
            %%todo: 返回的要整理
            case Mod:handle_http(Conf, Payload) of
                ok -> {200, <<"">>};
                {ok, _} -> {200, <<"">>};
                {error, Code} -> {Code, <<"">>}
            end
    end.

get_push_conf(EpushId) ->
    eutil:get_ets(epush_confs, {push_confs, EpushId}).
