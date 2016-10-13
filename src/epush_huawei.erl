-module(epush_huawei).
-export([handle_send/2,
         loop/4, handle_info/2
        ]).
-include("epush.hrl").
-include_lib("huawei_push/include/huawei_push.hrl").


handle_send(MQPayload, #{access_token:=AccessToken}) ->
    PayloadMaps = jiffy:decode(MQPayload, [return_maps]),
    case maps:get(<<"push_method">>, PayloadMaps, undefined) of
        <<"single_send">> ->
            NewPayload = maps:merge(?HW_SINGLE_ARGS#{<<"access_token">> => AccessToken}, maps:remove(<<"push_method">>, PayloadMaps)),
            huawei_push:send(NewPayload);
        %%todo: 要不要拆开tokens，全部人，tags三个接口处理
        <<"notification_send">> ->
            #{<<"title">> := Title, <<"content">> := Content} = PayloadMaps,
            AndroidMsg = jiffy:encode(#{<<"notification_title">> => Title, <<"notification_content">> => Content, <<"doings">> => 1}),
            NewPayload = maps:merge(?HW_NOTIFICATION_ARGS#{<<"access_token">> => AccessToken, <<"android">> => AndroidMsg}, maps:without([<<"push_method">>, <<"title">>, <<"content">>], PayloadMaps)),
            huawei_push:send(NewPayload);
        <<"batch_send">> ->
            DeviceTokenListOri = maps:get(<<"deviceTokenList">>, PayloadMaps, []),
            DeviceTokenList = [binary_to_list(Item) || Item <- DeviceTokenListOri],
            NewList = lists:flatten(io_lib:format("~p", [DeviceTokenList])),
            NewPayloadMaps = PayloadMaps#{<<"deviceTokenList">> => NewList},
            NewPayload = maps:merge(?HW_BATCH_ARGS#{<<"access_token">> => AccessToken}, maps:remove(<<"push_method">>, NewPayloadMaps)),
            huawei_push:send(NewPayload);
        undefined ->
            #{<<"token">> := DeviceToken, <<"content">> := Content} = PayloadMaps,
            NewPayload = maps:merge(?HW_SINGLE_ARGS#{<<"access_token">> => AccessToken, <<"deviceToken">> => DeviceToken, <<"message">> => Content}, maps:remove(<<"push_method">>, PayloadMaps)),
            huawei_push:send(NewPayload)
    end.


loop(_RoutingKey, _ContentType, Payload, #{app_id:=AppId, app_secret:=AppSecret}=State) ->
    NewState = case maps:get(access_token, State, undefined) of
                   undefined ->
                       #{<<"access_token">> := AccessToken,
                         <<"expires_in">> := ExpireIn} = huawei_push:get_access_token_info(AppId, AppSecret),
                       handle_refresh_access_token(ExpireIn),
                       State#{access_token => AccessToken};
                   _ ->
                       State
               end,
    case handle_send(Payload, NewState) of
        {ok, _} ->
            {ack, NewState};
        {access_token_expire, _} ->
            self() ! refresh_access_token_now,
            {reject, NewState};
        error ->
            {remove, NewState}
    end.

handle_refresh_access_token(ExpireIn) ->
    Time = case ExpireIn > 700 of
               true -> (ExpireIn - 600 - rand:uniform(100)) * 1000;
               false -> ExpireIn * 1000
           end,
    erlang:send_after(Time, self(), refresh_access_token).

handle_info(refresh_access_toke_now, #{app_id:=AppId, app_secret:=AppSecret}=State) ->
    #{<<"access_token">> := AccessToken} = huawei_push:get_access_token_info(AppId, AppSecret),
    {ok, State#{access_token => AccessToken}};
handle_info(refresh_access_token, #{app_id:=AppId, app_secret:=AppSecret}=State) ->
    #{<<"access_token">> := AccessToken,
      <<"expires_in">> := ExpireIn} = huawei_push:get_access_token_info(AppId, AppSecret),
    handle_refresh_access_token(ExpireIn),
    {ok, State#{access_token => AccessToken}};
handle_info(_Info, State) ->
    {ok, State}.





