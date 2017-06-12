-module(epush_hms).
-export([handle_http/2,
         loop/4, handle_info/2
        ]).
-include_lib("hms_push/include/hms_push.hrl").
-include_lib("eutil/include/eutil.hrl").

handle_push(AccessToken, #{<<"push_method">> := <<"general_notification">>,
                           <<"device_token">> := DeviceToken,
                           <<"title">> := Title, <<"content">> := Content}) ->
    hms_push:general_notification(AccessToken, DeviceToken, Title, Content);

handle_push(AccessToken, #{<<"push_method">> := <<"general_app_msg">>,
                           <<"device_token">> := DeviceToken,
                           <<"msg">> := Msg}) ->
    hms_push:general_app_msg(AccessToken, DeviceToken, Msg);

handle_push(AccessToken, #{<<"push_method">> := <<"single_send">>} = PayloadMaps) ->
            NewPayload = maps:merge(?HMS_SINGLE_ARGS#{<<"access_token">> => AccessToken},
                                    maps:remove(<<"push_method">>, PayloadMaps)),
            hms_push:send(NewPayload);

%%todo: 要不要拆开tokens，全部人，tags三个接口处理
handle_push(AccessToken, #{<<"push_method">> := <<"notification_send">>, <<"title">> := Title,
                           <<"content">> := Content} = PayloadMaps) ->
    AndroidMsg = eutil:json_encode(#{<<"notification_title">> => Title,
                                <<"notification_content">> => Content, <<"doings">> => 1}),
    NewPayload = maps:merge(?HMS_PS_SINGLE_ARGS#{<<"access_token">> => AccessToken, <<"android">> => AndroidMsg},
                            maps:without([<<"push_method">>, <<"title">>, <<"content">>], PayloadMaps)),
    hms_push:send(NewPayload);

handle_push(AccessToken, #{<<"push_method">> := <<"batch_send">>} = PayloadMaps) ->
    DeviceTokenListOri = maps:get(<<"deviceTokenList">>, PayloadMaps, []),
    DeviceTokenList = [binary_to_list(Item) || Item <- DeviceTokenListOri],
    NewList = lists:flatten(io_lib:format("~p", [DeviceTokenList])),
    NewPayloadMaps = PayloadMaps#{<<"deviceTokenList">> => NewList},
    NewPayload = maps:merge(?HMS_BATCH_ARGS#{<<"access_token">> => AccessToken},
                            maps:remove(<<"push_method">>, NewPayloadMaps)),
    hms_push:send(NewPayload);

handle_push(AccessToken, #{<<"push_method">> := <<"ps_batch_send">>} = PayloadMaps) ->
    DeviceTokenListOri = maps:get(<<"deviceTokenList">>, PayloadMaps, []),
    DeviceTokenList = [binary_to_list(Item) || Item <- DeviceTokenListOri],
    NewList = lists:flatten(io_lib:format("~p", [DeviceTokenList])),
    NewPayloadMaps = PayloadMaps#{<<"deviceTokenList">> => NewList},
    NewPayload = maps:merge(?HMS_PS_BATCH_ARGS#{<<"access_token">> => AccessToken},
                            maps:remove(<<"push_method">>, NewPayloadMaps)),
    hms_push:send(NewPayload);

handle_push(AccessToken, #{<<"token">> := DeviceToken, <<"content">> := Content}) ->
    Payload = ?HMS_SINGLE_ARGS#{<<"access_token">> => AccessToken, <<"deviceToken">> => DeviceToken,
                                <<"message">> => Content},
    hms_push:send(Payload).


loop(_RoutingKey, _ContentType, Payload, #{app_id:=AppId, app_secret:=AppSecret}=State) ->
    NewState = case maps:get(access_token, State, undefined) of
                   undefined ->
                       #{<<"access_token">> := AccessToken,
                         <<"expires_in">> := ExpireIn} = hms_push:get_access_token_info(AppId, AppSecret),
                       handle_refresh_access_token(ExpireIn),
                       State#{access_token => AccessToken};
                   _ ->
                       State
               end,
    case handle_push(maps:get(access_token, NewState), eutil:json_decode(Payload)) of
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
    #{<<"access_token">> := AccessToken} = hms_push:get_access_token_info(AppId, AppSecret),
    {ok, State#{access_token => AccessToken}};
handle_info(refresh_access_token, #{app_id:=AppId, app_secret:=AppSecret}=State) ->
    #{<<"access_token">> := AccessToken,
      <<"expires_in">> := ExpireIn} = hms_push:get_access_token_info(AppId, AppSecret),
    handle_refresh_access_token(ExpireIn),
    {ok, State#{access_token => AccessToken}};
handle_info(_Info, State) ->
    {ok, State}.

handle_http(#{app_id:=AppId, app_secret:=AppSecret}, Payload) ->
    AccessToken = case eutil:get_ets(epush_confs, hms_access_token_info) of
                      undefined ->
                          TokenInfo = hms_push:get_access_token_info(AppId, AppSecret),
                          NewInfo = TokenInfo#{<<"start">> => erlang:system_time(seconds)},
                          eutil:put_ets(epush_confs, hms_access_token_info, NewInfo),
                          maps:get(<<"access_token">>, NewInfo);
                      #{<<"access_token">> := Token, <<"expires_in">> := ExpireIn, <<"start">> := Start} ->
                           Now = erlang:system_time(seconds),
                           case Now - Start >= ExpireIn - 6 of
                               true ->
                                   TokenInfo = hms_push:get_access_token_info(AppId, AppSecret),
                                   NewInfo = TokenInfo#{<<"start">> => erlang:system_time(seconds)},
                                   eutil:put_ets(epush_confs, hms_access_token_info, NewInfo),
                                   maps:get(<<"access_token">>, NewInfo);
                               false ->
                                   Token
                           end
                  end,
    handle_push(AccessToken, Payload).
