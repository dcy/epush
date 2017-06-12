-module(epush_flyme).
-export([handle_http/2,
         loop/4, handle_info/2
        ]).

-include_lib("eutil/include/eutil.hrl").

%%todo: tags
handle_push(AppId, AppSecret, #{<<"push_method">> := <<"general_notification">>,
                                <<"device_token">> := DeviceToken,
                                <<"title">> := Title, <<"content">> := Content}) ->
    flyme_push:general_notification(AppId, AppSecret, DeviceToken, Title, Content);

handle_push(AppId, AppSecret, #{<<"push_method">> := <<"general_app_msg">>,
                                <<"device_token">> := DeviceToken, <<"msg">> := Msg}) ->
    flyme_push:general_app_msg(AppId, AppSecret, DeviceToken, Msg);

handle_push(AppId, AppSecret, #{<<"push_method">> := <<"unvarnished">>, <<"content">> := Content} = PayloadMaps) ->
    URL = <<"http://api-push.meizu.com/garcia/api/server/push/unvarnished/pushByPushId">>,
    MessageJson = eutil:json_encode(#{<<"content">> => Content}),
    RemovedMaps = maps:remove(<<"push_method">>, PayloadMaps),
    NewPayload =  RemovedMaps#{<<"messageJson">> => MessageJson},
    flyme_push:send(AppId, AppSecret, URL, NewPayload);

handle_push(AppId, AppSecret, #{<<"push_method">> := <<"notification">>, <<"title">> := Title,
                                <<"content">> := Content} = PayloadMaps) ->
    URL = <<"http://api-push.meizu.com/garcia/api/server/push/varnished/pushByPushId">>,
    #{<<"title">> := Title, <<"content">> := Content} = PayloadMaps,
    MessageJson = eutil:json_encode(#{<<"noticeBarInfo">> => #{<<"noticeBarType">> => 0, <<"title">> => Title, <<"content">> => Content}}),
    WithoutMaps = maps:without([<<"push_method">>, <<"title">>, <<"content">>], PayloadMaps),
    NewPayload = WithoutMaps#{<<"messageJson">> => MessageJson},
    flyme_push:send(AppId, AppSecret, URL, NewPayload);

handle_push(AppId, AppSecret, #{<<"push_method">> := <<"varnished">>, <<"title">> := Title, 
                                <<"content">> := Content} = PayloadMaps) ->
    URL = <<"http://api-push.meizu.com/garcia/api/server/push/varnished/pushByPushId">>,
    #{<<"title">> := Title, <<"content">> := Content} = PayloadMaps,
    MessageJson = eutil:json_encode(#{<<"noticeBarInfo">> => #{<<"noticeBarType">> => 0, <<"title">> => Title, <<"content">> => Content}}),
    WithoutMaps = maps:without([<<"push_method">>, <<"title">>, <<"content">>], PayloadMaps),
    NewPayload =  WithoutMaps#{<<"messageJson">> => MessageJson},
    flyme_push:send(AppId, AppSecret, URL, NewPayload);

handle_push(AppId, AppSecret, #{<<"token">> := PushId, <<"content">> := Content}) ->
    %URL = <<"http://api-push.meizu.com/garcia/api/server/push/unvarnished/pushByPushId">>,
    %MessageJson = jiffy:encode(#{<<"content">> => Content}),
    %Payload = #{<<"pushIds">> => PushId, <<"messageJson">> => MessageJson},
    %flyme_push:send(AppId, AppSecret, URL, Payload).
    flyme_push:general_app_msg(AppId, AppSecret, PushId, Content).


loop(_RoutingKey, _ContentType, Payload, State) ->
    handle_mq(State, Payload),
    {ack, State}.

handle_info(_Info, State) ->
    {ok, State}.

handle_mq(#{app_id := AppId, app_secret := AppSecret}, Payload) ->
    handle_push(AppId, AppSecret, eutil:json_decode(Payload)).


handle_http(#{app_id := AppId, app_secret := AppSecret}, Payload) ->
    handle_push(AppId, AppSecret, Payload).
