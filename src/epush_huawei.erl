-module(epush_huawei).
-export([get_access_token/2, handle_send/2,
         loop/4, handle_info/2
        ]).
-include("epush.hrl").

%%return code
-define(SUCCESS, 0).
-define(ACCESS_TOKEN_EXPIRE, 6).

-define(URL, <<"https://api.vmall.com/rest.php">>).
-define(HEADERS, [{<<"Content-Type">>, <<"application/x-www-form-urlencoded; charset=utf-8">>}]).

-define(SINGLE_ARGS, #{<<"deviceToken">> => "", <<"message">> => <<"message">>, <<"priority">> => 1,
                       <<"nsp_svc">> => <<"openpush.message.single_send">>, 
                       <<"nsp_ts">> => erlang:system_time(seconds),
                       <<"cacheMode">> => 0, <<"msgType">> => rand:uniform(100)}).

-define(NOTIFICATION_ARGS, #{<<"push_type">> => 1,
                             <<"nsp_ts">> => erlang:system_time(seconds),
                             <<"nsp_svc">> => <<"openpush.openapi.notification_send">>}).

-define(BATCH_ARGS, #{<<"nsp_svc">> => <<"openpush.message.batch_send">>,
                      <<"nsp_ts">> => erlang:system_time(seconds),
                      <<"cacheMode">> => 0, <<"msgType">> => rand:uniform(100)}).

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
    PayloadMaps = jiffy:decode(MQPayload, [return_maps]),
    case maps:get(<<"push_method">>, PayloadMaps, undefined) of
        <<"single_send">> ->
            NewPayload = maps:merge(?SINGLE_ARGS#{<<"access_token">> => AccessToken}, maps:remove(<<"push_method">>, PayloadMaps)),
            do_send(NewPayload);
        %%todo: 要不要拆开tokens，全部人，tags三个接口处理
        <<"notification_send">> ->
            #{<<"title">> := Title, <<"content">> := Content} = PayloadMaps,
            AndroidMsg = jiffy:encode(#{<<"notification_title">> => Title, <<"notification_content">> => Content, <<"doings">> => 1}),
            NewPayload = maps:merge(?NOTIFICATION_ARGS#{<<"access_token">> => AccessToken, <<"android">> => AndroidMsg}, maps:without([<<"push_method">>, <<"title">>, <<"content">>], PayloadMaps)),
            do_send(NewPayload);
        <<"batch_send">> ->
            DeviceTokenListOri = maps:get(<<"deviceTokenList">>, PayloadMaps, []),
            DeviceTokenList = [binary_to_list(Item) || Item <- DeviceTokenListOri],
            NewList = lists:flatten(io_lib:format("~p", [DeviceTokenList])),
            NewPayloadMaps = PayloadMaps#{<<"deviceTokenList">> => NewList},
            NewPayload = maps:merge(?BATCH_ARGS#{<<"access_token">> => AccessToken}, maps:remove(<<"push_method">>, NewPayloadMaps)),
            do_send(NewPayload);
        undefined ->
            #{<<"token">> := DeviceToken, <<"content">> := Content} = PayloadMaps,
            NewPayload = maps:merge(?SINGLE_ARGS#{<<"access_token">> => AccessToken, <<"deviceToken">> => DeviceToken, <<"message">> => Content}, maps:remove(<<"push_method">>, PayloadMaps)),
            do_send(NewPayload)
    end.

do_send(PayloadMaps) ->
    Method = post,
    Payload = hackney_url:qs(maps:to_list(PayloadMaps)),
    %Payload = epush_util:urlencode(maps:to_list(PayloadMaps)),
    %Payload = jiffy:encode(PayloadMaps),
    Options = [{pool, default}],
    {ok, _StatusCode, _RespHeaders, ClientRef} = hackney:request(Method, ?URL, ?HEADERS,
                                                                 Payload, Options),
    {ok, ResultBin} = hackney:body(ClientRef),
    ResultOri = jiffy:decode(ResultBin, [return_maps]),
    Result = case erlang:is_map(ResultOri) of
                 true -> ResultOri;
                 false -> jiffy:decode(ResultOri, [return_maps])
             end,
    Code = case maps:get(<<"resultcode">>, Result, undefined) of
               undefined -> maps:get(<<"result_code">>, Result);
               Other -> Other
           end,
    case Code of
        ?SUCCESS ->
            ok;
        ?ACCESS_TOKEN_EXPIRE ->
            self() ! refresh_access_token_now,
            error;
        _ ->
            ?ERROR_MSG("epush_huawei error, PayloadMaps: ~p, Result: ~p", [PayloadMaps, Result]),
            ok
    end.



%notification_send(MQPayload, #{access_token:=AccessToken}) ->
%    ?TRACE_VAR(notification_send),
%    #{<<"token">> := DeviceToken, <<"content">> := Content} = jiffy:decode(MQPayload, [return_maps]),
%    AndroidMsg = jiffy:encode(#{notification_title=> <<"Hello">>, notification_content => <<"world">>, doings => 1}),
%    Datas = [{push_type, 1}, {tokens, DeviceToken}, {android, AndroidMsg}, {access_token, AccessToken},
%             {nsp_svc, "openpush.openapi.notification_send"}, {nsp_ts, erlang:system_time(seconds)}
%            ],
%    Method = post,
%    Headers = [{<<"Content-Type">>, <<"application/x-www-form-urlencoded; charset=utf-8">>}],
%    URL = <<"https://api.vmall.com/rest.php">>,
%    Payload = epush_util:urlencode(Datas),
%    Options = [{pool, default}],
%    {ok, _StatusCode, _RespHeaders, ClientRef} = hackney:request(Method, URL, Headers,
%                                                                 Payload, Options),
%    {ok, ResultBin} = hackney:body(ClientRef),
%    Result = jiffy:decode(jiffy:decode(ResultBin, [return_maps]), [return_maps]),
%    case maps:get(<<"result_code">>, Result) of
%        0 ->
%            ok;
%        6 ->
%            self() ! refresh_access_token_now,
%            error;
%        _ ->
%            ?ERROR_MSG("epush huawei error, deviceToken: ~p, Result: ~p", [DeviceToken, Result]),
%            error
%    end.

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
