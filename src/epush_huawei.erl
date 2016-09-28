-module(epush_huawei).
-export([get_access_token/2, handle_send/2,
         loop/4, handle_info/2
        ]).
-include("epush.hrl").

get_access_token(AppId, AppSecret) ->
    Datas = [{grant_type, "client_credentials"}, {client_id, AppId},
             {client_secret, AppSecret}],
    Method = post,
    Headers = [{<<"Content-Type">>, <<"application/x-www-form-urlencoded; charset=utf-8">>}],
    URL = <<"https://login.vmall.com/oauth2/token">>,
    Payload = epush_util:urlencode(Datas),
    Options = [{pool, default}],
    {ok, _StatusCode, _RespHeaders, ClientRef} = hackney:request(Method, URL, Headers,
                                                                 Payload, Options),
    {ok, ResultBin} = hackney:body(ClientRef),
    jiffy:decode(ResultBin, [return_maps]).

handle_send(MQPayload, #{access_token:=AccessToken}) ->
    #{<<"token">> := DeviceToken, <<"content">> := Content} = jiffy:decode(MQPayload, [return_maps]),
    Datas = [{deviceToken, DeviceToken}, {message, Content},
             {nsp_svc, "openpush.message.single_send"}, {nsp_ts, erlang:system_time(seconds)},
             {priority, 0}, {cacheMode, 1}, {msgType, 1},
             {access_token, AccessToken}
            ],
    Method = post,
    Headers = [{<<"Content-Type">>, <<"application/x-www-form-urlencoded; charset=utf-8">>}],
    URL = <<"https://api.vmall.com/rest.php">>,
    Payload = epush_util:urlencode(Datas),
    Options = [{pool, default}],
    {ok, _StatusCode, _RespHeaders, ClientRef} = hackney:request(Method, URL, Headers,
                                                                 Payload, Options),
    {ok, ResultBin} = hackney:body(ClientRef),
    Result = jiffy:decode(ResultBin, [return_maps]),
    case maps:get(<<"resultcode">>, Result) of
        0 ->
            ok;
        6 ->
            self() ! refresh_access_token_now,
            error;
        _ ->
            ?ERROR_MSG("epush huawei error, deviceToken: ~p, Result: ~p", [DeviceToken, Result]),
            error
    end.

notification_send(MQPayload, #{access_token:=AccessToken}) ->
    #{<<"token">> := DeviceToken, <<"content">> := Content} = jiffy:decode(MQPayload, [return_maps]),
    AndroidMsg = jiffy:encode(#{notification_title=> <<"Hello">>, notification_content => <<"world">>, doings => 1}),
    Datas = [{push_type, 1}, {tokens, DeviceToken}, {android, AndroidMsg}, {access_token, AccessToken},
             {nsp_svc, "openpush.openapi.notification_send"}, {nsp_ts, erlang:system_time(seconds)}
            ],
    Method = post,
    Headers = [{<<"Content-Type">>, <<"application/x-www-form-urlencoded; charset=utf-8">>}],
    URL = <<"https://api.vmall.com/rest.php">>,
    Payload = epush_util:urlencode(Datas),
    Options = [{pool, default}],
    {ok, _StatusCode, _RespHeaders, ClientRef} = hackney:request(Method, URL, Headers,
                                                                 Payload, Options),
    {ok, ResultBin} = hackney:body(ClientRef),
    Result = jiffy:decode(jiffy:decode(ResultBin, [return_maps]), [return_maps]),
    case maps:get(<<"result_code">>, Result) of
        0 ->
            ok;
        6 ->
            self() ! refresh_access_token_now,
            error;
        _ ->
            ?ERROR_MSG("epush huawei error, deviceToken: ~p, Result: ~p", [DeviceToken, Result]),
            error
    end.

loop(_RoutingKey, _ContentType, Payload, #{app_id:=AppId, app_secret:=AppSecret}=State) ->
    NewState = case maps:get(access_token, State, undefined) of
                   undefined ->
                       #{<<"access_token">> := AccessToken,
                         <<"expires_in">> := ExpireIn} = get_access_token(AppId, AppSecret),
                       handle_refresh_access_token(ExpireIn),
                       State#{access_token => AccessToken};
                   _ ->
                       State
               end,
    case handle_send(Payload, NewState) of
        ok -> {ack, NewState};
        _ -> {reject, NewState}
    end.

handle_refresh_access_token(ExpireIn) ->
    Time = case ExpireIn > 700 of
               true -> (ExpireIn - 600 - rand:uniform(100)) * 1000;
               false -> ExpireIn * 1000
           end,
    erlang:send_after(Time, self(), refresh_access_token).

handle_info(refresh_access_toke_now, #{app_id:=AppId, app_secret:=AppSecret}=State) ->
    #{<<"access_token">> := AccessToken} = get_access_token(AppId, AppSecret),
    {ok, State#{access_token => AccessToken}};
handle_info(refresh_access_token, #{app_id:=AppId, app_secret:=AppSecret}=State) ->
    #{<<"access_token">> := AccessToken,
      <<"expires_in">> := ExpireIn} = get_access_token(AppId, AppSecret),
    handle_refresh_access_token(ExpireIn),
    {ok, State#{access_token => AccessToken}};
handle_info(_Info, State) ->
    {ok, State}.














send_msg() ->
    Datas = [{deviceToken, "_c9d1ee9a52354742000002296000001"}, {message, unicode:characters_to_binary("中文")},
             {nsp_svc, "openpush.message.single_send"}, {nsp_ts, erlang:system_time(seconds)},
             {priority, 1}, {cacheMode, 1}, {msgType, 1},
             {access_token, get_access_token(sss, dsdf)}
            ],
    Method = post,
    Headers = [{<<"Content-Type">>, <<"application/x-www-form-urlencoded; charset=utf-8">>}],
    URL = <<"https://api.vmall.com/rest.php">>,
    Payload = epush_util:urlencode(Datas),
    Options = [{pool, default}],
    {ok, StatusCode, RespHeaders, ClientRef} = hackney:request(Method, URL, Headers,
                                                               Payload, Options),
    ?TRACE_VAR(StatusCode),
    ?TRACE_VAR(RespHeaders),
    {ok, ResultBin} = hackney:body(ClientRef),
    ?TRACE_VAR(ResultBin),
    Result = jiffy:decode(ResultBin, [return_maps]),
    ?TRACE_VAR(Result),
    ok.
