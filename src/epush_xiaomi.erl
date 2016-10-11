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
        <<"notification_send">> ->
            URL = <<"https://api.xmpush.xiaomi.com/v3/message/regid">>,
            NewPayload = maps:merge(?PAYLOAD_MAPS#{<<"restricted_package_name">> => PkgName, <<"pass_through">> => 0}, maps:remove(<<"push_method">>, PayloadMaps)),
            do_send(URL, NewPayload, Headers);
        <<"pass_through">> ->
            URL = <<"https://api.xmpush.xiaomi.com/v3/message/regid">>,
            NewPayload = maps:merge(?PAYLOAD_MAPS#{<<"restricted_package_name">> => PkgName, <<"pass_through">> => 1}, maps:remove(<<"push_method">>, PayloadMaps)),
            do_send(URL, NewPayload, Headers);
        <<"all">> ->
            URL = <<"https://api.xmpush.xiaomi.com/v3/message/all">>,
            NewPayload = maps:merge(?PAYLOAD_MAPS#{<<"restricted_package_name">> => PkgName}, maps:remove(<<"push_method">>, PayloadMaps)),
            do_send(URL, NewPayload, Headers);
        <<"topic">> ->
            URL = <<"https://api.xmpush.xiaomi.com/v3/message/topic">>,
            NewPayload = maps:merge(?PAYLOAD_MAPS#{<<"restricted_package_name">> => PkgName}, maps:remove(<<"push_method">>, PayloadMaps)),
            do_send(URL, NewPayload, Headers);
        <<"multi_topic">> ->
            URL = <<"https://api.xmpush.xiaomi.com/v3/message/multi_topic">>,
            NewPayload = maps:merge(?PAYLOAD_MAPS#{<<"restricted_package_name">> => PkgName}, maps:remove(<<"push_method">>, PayloadMaps)),
            do_send(URL, NewPayload, Headers)
    end.

do_send(URL, PayloadMaps, Headers) ->
    Method = post,
    Payload = epush_util:urlencode(maps:to_list(PayloadMaps)),
    Options = [{pool, default}],
    {ok, _StatusCode, _RespHeaders, ClientRef} = hackney:request(Method, URL, Headers,
                                                                 Payload, Options),
    {ok, ResultBin} = hackney:body(ClientRef),
    Result = jiffy:decode(ResultBin, [return_maps]),
    #{<<"code">>:=Code} = Result,
    case Code of
        0 ->
            ok;
        _ ->
            ?ERROR_MSG("epush xiaomi error, URL: ~p, PayloadMaps: ~p, Result: ~p", [URL, PayloadMaps, Result]),
            error
    end.


loop(_RoutingKey, _ContentType, Payload, State) ->
    handle_send(Payload, State),
    {ack, State}.

handle_info(_Info, State) ->
    {ok, State}.

