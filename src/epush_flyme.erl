-module(epush_flyme).
-export([handle_send/2,
         loop/4, handle_info/2
        ]).
-include("epush.hrl").

-define(HEADERS, [{<<"Content-Type">>, <<"application/x-www-form-urlencoded; charset=utf-8">>}]).


handle_send(MQPayload, #{app_id:=AppId, app_secret:=AppSecret}) ->
    PayloadMaps = jiffy:decode(MQPayload, [return_maps]),
    {URL, NewPayload} = case maps:get(<<"push_method">>, PayloadMaps, undefined) of
                            <<"unvarnished">> ->
                                UnvarnishedURL = <<"http://api-push.meizu.com/garcia/api/server/push/unvarnished/pushByPushId">>,
                                #{<<"content">> := Content} = PayloadMaps,
                                MessageJson = jiffy:encode(#{<<"content">> => Content}),
                                RemovedMaps = maps:remove(<<"push_method">>, PayloadMaps),
                                {UnvarnishedURL, RemovedMaps#{<<"messageJson">> => MessageJson}};
                            <<"notification">> ->
                                VarnishedURL = <<"http://api-push.meizu.com/garcia/api/server/push/varnished/pushByPushId">>,
                                #{<<"title">> := Title, <<"content">> := Content} = PayloadMaps,
                                MessageJson = jiffy:encode(#{<<"noticeBarInfo">> => #{<<"noticeBarType">> => 0, <<"title">> => Title, <<"content">> => Content}}),
                                WithoutMaps = maps:without([<<"push_method">>, <<"title">>, <<"content">>], PayloadMaps),
                                {VarnishedURL, WithoutMaps#{<<"messageJson">> => MessageJson}};
                            <<"varnished">> ->
                                VarnishedURL = <<"http://api-push.meizu.com/garcia/api/server/push/varnished/pushByPushId">>,
                                #{<<"title">> := Title, <<"content">> := Content} = PayloadMaps,
                                MessageJson = jiffy:encode(#{<<"noticeBarInfo">> => #{<<"noticeBarType">> => 0, <<"title">> => Title, <<"content">> => Content}}),
                                WithoutMaps = maps:without([<<"push_method">>, <<"title">>, <<"content">>], PayloadMaps),
                                {VarnishedURL, WithoutMaps#{<<"messageJson">> => MessageJson}};
                            undefined ->
                                #{<<"token">> := PushId, <<"content">> := Content} = PayloadMaps,
                                UnvarnishedURL = <<"http://api-push.meizu.com/garcia/api/server/push/unvarnished/pushByPushId">>,
                                MessageJson = jiffy:encode(#{<<"content">> => Content}),
                                Payload = #{<<"pushIds">> => PushId, <<"messageJson">> => MessageJson},
                                {UnvarnishedURL, Payload}
                        end,
    flyme_push:send(AppId, AppSecret, URL, NewPayload).


loop(_RoutingKey, _ContentType, Payload, State) ->
    handle_send(Payload, State),
    {ack, State}.

handle_info(_Info, State) ->
    {ok, State}.

