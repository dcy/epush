-module(epush_xiaomi).
-export([handle_send/2,
         loop/4, handle_info/2
        ]).
-include("epush.hrl").

-define(PAYLOAD_MAPS, #{<<"title">> => "Hello", <<"description">> => "World", <<"pass_through">> => 0,
                        <<"payload">> => <<"">>, <<"restricted_package_name">> => "",
                        <<"registration_id">> => "", <<"extra.notify_effect">> => "1"}).


handle_send(MQPayload, #{pkg_name:=PkgName, headers:=Headers}) ->
    PayloadMaps = jiffy:decode(MQPayload, [return_maps]),
    case maps:get(<<"push_method">>, PayloadMaps) of
        "notification_send" ->
            NewPayload = maps:merge(?PAYLOAD_MAPS#{<<"restricted_package_name">> => PkgName, <<"pass_through">> => 0}, maps:remove(<<"push_method">>, PayloadMaps)),
            do_send(NewPayload, Headers);
        <<"pass_through">> ->
            NewPayload = maps:merge(?PAYLOAD_MAPS#{<<"restricted_package_name">> => PkgName, <<"pass_through">> => 1}, maps:remove(<<"push_method">>, PayloadMaps)),
            do_send(NewPayload, Headers)
    end.

do_send(PayloadMaps, Headers) ->
    Method = post,
    URL = <<"https://api.xmpush.xiaomi.com/v3/message/regid">>,
    Payload = epush_util:urlencode(maps:to_list(PayloadMaps)),
    Options = [{pool, default}],
    {ok, _StatusCode, _RespHeaders, ClientRef} = hackney:request(Method, URL, Headers,
                                                                 Payload, Options),
    {ok, ResultBin} = hackney:body(ClientRef),
    Result = jiffy:decode(ResultBin, [return_maps]),
    #{<<"code">>:=Code} = Result,
    case Code of
        0 ->
            ?INFO_MSG("epush_xiaomi pass_through_send success push: PayloadMaps: ~p", [PayloadMaps]),
            ok;
        _ ->
            ?ERROR_MSG("epush xiaomi error, PayloadMaps: ~p, Result: ~p", [PayloadMaps, Result]),
            error
    end.


loop(_RoutingKey, _ContentType, Payload, State) ->
    handle_send(Payload, State),
    {ack, State}.

handle_info(_Info, State) ->
    {ok, State}.

