-module(epush_xiaomi).
-export([handle_http/2, 
         loop/4, handle_info/2
        ]).

-define(PAYLOAD_MAPS, #{<<"title">> => "Hello", <<"description">> => "World",
                        <<"pass_through">> => 0, <<"notify_type">> => -1,
                        <<"payload">> => <<"">>, <<"restricted_package_name">> => "",
                        <<"registration_id">> => "", <<"extra.notify_effect">> => "1"}).


handle_push(PkgName, AppSecret, #{<<"push_method">> := <<"notification_send">>} = PayloadMaps) ->
    URL = <<"https://api.xmpush.xiaomi.com/v3/message/regid">>,
    NewMaps = maps:merge(?PAYLOAD_MAPS#{<<"restricted_package_name">> => PkgName, <<"pass_through">> => 0}, maps:remove(<<"push_method">>, PayloadMaps)),
    xiaomi_push:send(AppSecret, URL, NewMaps);

handle_push(PkgName, AppSecret, #{<<"push_method">> := <<"pass_through">>} = PayloadMaps) ->
    URL = <<"https://api.xmpush.xiaomi.com/v3/message/regid">>,
    NewMaps = maps:merge(?PAYLOAD_MAPS#{<<"restricted_package_name">> => PkgName, <<"pass_through">> => 1}, maps:remove(<<"push_method">>, PayloadMaps)),
    xiaomi_push:send(AppSecret, URL, NewMaps);

handle_push(PkgName, AppSecret, #{<<"push_method">> := <<"all">>} = PayloadMaps) ->
    URL = <<"https://api.xmpush.xiaomi.com/v3/message/all">>,
    NewMaps = maps:merge(?PAYLOAD_MAPS#{<<"restricted_package_name">> => PkgName}, maps:remove(<<"push_method">>, PayloadMaps)),
    xiaomi_push:send(AppSecret, URL, NewMaps);

handle_push(PkgName, AppSecret, #{<<"push_method">> := <<"topic">>} = PayloadMaps) ->
    URL = <<"https://api.xmpush.xiaomi.com/v3/message/topic">>,
    NewMaps = maps:merge(?PAYLOAD_MAPS#{<<"restricted_package_name">> => PkgName}, maps:remove(<<"push_method">>, PayloadMaps)),
    xiaomi_push:send(AppSecret, URL, NewMaps);

handle_push(PkgName, AppSecret, #{<<"push_method">> := <<"multi_topic">>} = PayloadMaps) ->
    URL = <<"https://api.xmpush.xiaomi.com/v3/message/multi_topic">>,
    NewMaps = maps:merge(?PAYLOAD_MAPS#{<<"restricted_package_name">> => PkgName}, maps:remove(<<"push_method">>, PayloadMaps)),
    xiaomi_push:send(AppSecret, URL, NewMaps);

handle_push(PkgName, AppSecret, #{<<"token">> := Token, <<"content">> := Content}) ->
    URL = <<"https://api.xmpush.xiaomi.com/v3/message/regid">>,
    NewMaps = ?PAYLOAD_MAPS#{<<"restricted_package_name">> => PkgName, <<"payload">> => Content,
                             <<"pass_through">> => 1, <<"registration_id">> => Token
                            },
    xiaomi_push:send(AppSecret, URL, NewMaps).



loop(_RoutingKey, _ContentType, Payload, State) ->
    handle_mq(State, Payload),
    {ack, State}.

handle_info(_Info, State) ->
    {ok, State}.

handle_http(Conf, Payload) ->
    #{pkg_name := PkgName, app_secret := AppSecret} = Conf,
    Result = handle_push(PkgName, AppSecret, Payload),
    Result.

handle_mq(#{pkg_name := PkgName, app_secret := AppSecret}, Payload) ->
    Result = handle_push(PkgName, AppSecret, jiffy:decode(Payload, [return_maps])),
    Result.
