-module(epush_flyme).
-export([handle_send/2,
         loop/4, handle_info/2
        ]).
-include("epush.hrl").

-define(HEADERS, [{<<"Content-Type">>, <<"application/x-www-form-urlencoded; charset=utf-8">>}]).


handle_send(MQPayload, #{app_id:=AppId, app_secret:=AppSecret}) ->
    PayloadMaps = jiffy:decode(MQPayload, [return_maps]),
    case maps:get(<<"push_method">>, PayloadMaps, undefined) of
		<<"unvarnished">> ->
            URL = <<"http://api-push.meizu.com/garcia/api/server/push/unvarnished/pushByPushId">>,
            #{<<"content">> := Content} = PayloadMaps,
            MessageJson = jiffy:encode(#{<<"content">> => Content}),
            RemovedMaps = maps:remove(<<"push_method">>, PayloadMaps),
            NewPayload = RemovedMaps#{<<"messageJson">> => MessageJson},
            do_send(URL, NewPayload, AppId, AppSecret),
            ok;
        <<"varnished">> ->
            URL = <<"http://api-push.meizu.com/garcia/api/server/push/varnished/pushByPushId">>,
            #{<<"title">> := Title, <<"content">> := Content} = PayloadMaps,
            MessageJson = jiffy:encode(#{<<"noticeBarInfo">> => #{<<"noticeBarType">> => 0, <<"title">> => Title, <<"content">> => Content}}),
            WithoutMaps = maps:without([<<"push_method">>, <<"title">>, <<"content">>], PayloadMaps),
            NewPayload = WithoutMaps#{<<"messageJson">> => MessageJson},
            do_send(URL, NewPayload, AppId, AppSecret);
        undefined ->
            #{<<"token">> := PushId, <<"content">> := Content} = PayloadMaps,
            URL = <<"http://api-push.meizu.com/garcia/api/server/push/unvarnished/pushByPushId">>,
            MessageJson = jiffy:encode(#{<<"content">> => Content}),
            Payload = #{<<"pushIds">> => PushId, <<"messageJson">> => MessageJson},
            do_send(URL, Payload, AppId, AppSecret)
    end.

do_send(URL, PayloadMaps, AppId, AppSecret) ->
    AppIdMaps = PayloadMaps#{<<"appId">> => AppId},
    Sign = format_sign(AppIdMaps, AppSecret),
    SignMaps = AppIdMaps#{<<"sign">> => Sign},
    Method = post,
    Payload = epush_util:urlencode(SignMaps),
    Options = [{pool, flyme}],
    {ok, _StatusCode, _RespHeaders, ClientRef} = hackney:request(Method, URL, ?HEADERS,
                                                                 Payload, Options),
    {ok, ResultBin} = hackney:body(ClientRef),
    Result = jiffy:decode(ResultBin, [return_maps]),
    %%todo: 1003服务器忙要不要返回队列
    case maps:get(<<"code">>, Result) of
        <<"200">> ->
            ok;
        _ ->
            ?ERROR_MSG("epush_flyme error, PayloadMaps:~p, Result:~p", [SignMaps, Result]),
            ok
    end.


loop(_RoutingKey, _ContentType, Payload, State) ->
    handle_send(Payload, State),
    {ack, State}.

handle_info(_Info, State) ->
    {ok, State}.

format_sign(Maps, AppSecret) ->
    Fun = fun({K, V}, TempStr) ->
                  TempStr ++ epush_util:to_list(K) ++ "=" ++ epush_util:to_list(V)
          end,
    KvStr = lists:foldl(Fun, "", lists:sort(maps:to_list(Maps))),
    epush_util:md5_hex(KvStr ++ AppSecret).

test_unvarnished() ->
    AppId = 110025,
	AppSecret = <<"ef3fc31b3e15473e9fcc7afb00d41a4d">>,
    MessageJson = jiffy:encode(#{<<"content">> => <<"Content...">>}),
    PayloadList = [{<<"appId">>, AppId}, {<<"pushIds">>, <<"UU34b4f75595d58540a78407f4d5a60630642497c5c5e">>}, {<<"messageJson">>, MessageJson}],
    Sign = format_sign(lists:sort(PayloadList), AppSecret),
    Method = post,
    URL = <<"http://api-push.meizu.com/garcia/api/server/push/unvarnished/pushByPushId">>,
    %Payload = jiffy:encode({PayloadList ++ [{<<"sign">>, list_to_binary(Sign)}]}),
    Payload = epush_util:urlencode(PayloadList ++ [{<<"sign">>, list_to_binary(Sign)}]),
    ?TRACE_VAR(Payload),
    Options = [{pool, flyme}],
    {ok, _StatusCode, _RespHeaders, ClientRef} = hackney:request(Method, URL, ?HEADERS,
                                                                 Payload, Options),
    {ok, ResultBin} = hackney:body(ClientRef),
    Result = jiffy:decode(ResultBin, [return_maps]),
    ?TRACE_VAR(Result),
    ok.
