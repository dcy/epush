-module(epush_fcm).
-export([handle_http/2,
         loop/4, handle_info/2
        ]).

-include_lib("eutil/include/eutil.hrl").

%%todo: fcm_push:format_multi_topics
format_multi_topics(Topics) ->
    Fun = fun(Topic) ->
                  binary_to_list(<<"'", Topic/binary, "' in topics">>)
          end,
    TopicList = lists:map(Fun, Topics),
    string:join(TopicList, " || ").

handle_push(ApiKey, Proxy, #{<<"push_method">> := <<"general_notification">>,
                             <<"device_token">> := DeviceToken,
                             <<"title">> := Title, <<"content">> := Content}) ->
    fcm_push:general_notification(ApiKey, Proxy, DeviceToken, Title, Content);

handle_push(ApiKey, Proxy, #{<<"push_method">> := <<"general_app_msg">>,
                             <<"device_token">> := DeviceToken, <<"msg">> := Msg}) ->
    fcm_push:general_app_msg(ApiKey, Proxy, DeviceToken, #{content => Msg});

handle_push(ApiKey, Proxy, #{<<"push_method">> := <<"notification">>, <<"title">> := Title,
                             <<"content">> := Body, <<"to">> := To}) ->
    Notification = #{<<"title">> => Title, <<"body">> => Body, <<"sound">> => <<"default">>},
    Msg = #{<<"to">> => To, <<"notification">> => Notification},
    fcm_push:send(ApiKey, Proxy, Msg);

handle_push(ApiKey, Proxy, #{<<"push_method">> := <<"data">>, <<"to">> := To,
                             <<"content">> := Content}) ->
    Data = #{<<"content">> => Content},
    Msg = #{<<"to">> => To, <<"data">> => Data},
    fcm_push:send(ApiKey, Proxy, Msg);

handle_push(ApiKey, Proxy, #{<<"push_method">> := <<"topics">>, <<"topics">> := Topics,
                             <<"content">> := Content}) ->
    Msg = case Topics of
              [Topic] ->%%单主题
                  #{<<"to">> => Topic, <<"data">> => Content};
              _ -> %%多主题
                  Condition = format_multi_topics(Topics),
                  #{<<"condition">> => Condition, <<"data">> => Content}
          end,
    fcm_push:send(ApiKey, Proxy, Msg);

handle_push(ApiKey, Proxy, #{<<"token">> := Token, <<"content">> := Content}) -> 
    fcm_push:general_app_msg(ApiKey, Proxy, Token, #{content => Content}).
    %Msg = #{to => Token, data => #{content => Content}},
    %fcm_push:send(ApiKey, Proxy, Msg).


loop(_RoutingKey, _ContentType, Payload, State) ->
    handle_mq(State, Payload),
    {ack, State}.

handle_info(_Info, State) ->
    {ok, State}.

handle_mq(#{api_key := ApiKey, proxy := Proxy}, Payload) ->
    handle_push(ApiKey, Proxy, eutil:json_decode(Payload)).

handle_http(#{api_key := ApiKey, proxy := Proxy}, Payload) ->
    handle_push(ApiKey, Proxy, Payload).
