-module(epush_fcm).
-export([handle_send/2,
         loop/4, handle_info/2
        ]).
-include("epush.hrl").

-define(URL, <<"https://fcm.googleapis.com/fcm/send">>).

%%todo: fcm_push:format_multi_topics
format_multi_topics(Topics) ->
    Fun = fun(Topic) ->
                  binary_to_list(<<"'", Topic/binary, "' in topics">>)
          end,
    TopicList = lists:map(Fun, Topics),
    string:join(TopicList, " || ").


handle_send(MQPayload, #{headers:=Headers, proxy:=Proxy}) ->
    PayloadMaps = jiffy:decode(MQPayload, [return_maps]),
    Msg = case maps:get(<<"push_method">>, PayloadMaps, undefined) of
              <<"notification">> ->
                  #{<<"title">> := Title, <<"content">> := Body, <<"to">> := To} = PayloadMaps,
                  Notification = #{<<"title">> => Title, <<"body">> => Body},
                  #{<<"to">> => To, <<"notification">> => Notification};
              <<"unvarnished">> ->
                  #{<<"to">> := To, <<"content">> := Content} = PayloadMaps,
                  Data = #{<<"content">> => Content},
                  #{<<"to">> => To, <<"data">> => Data};
              <<"topics">> ->
                  #{<<"topics">> := Topics, <<"content">> := Content} = PayloadMaps,
                  case Topics of
                      [Topic] ->%%单主题
                          #{<<"to">> => Topic, <<"data">> => Content};
                      _ -> %%多主题
                          Condition = format_multi_topics(Topics),
                          #{<<"condition">> => Condition, <<"data">> => Content}
                  end;
              undefined ->
                  #{<<"token">> := Token, <<"content">> := Content} = PayloadMaps,
                  #{to => Token, data => #{content => Content}}
          end,
    fcm_push:send(Msg, Headers, Proxy).

loop(_RoutingKey, _ContentType, Payload, State) ->
    handle_send(Payload, State),
    {ack, State}.

handle_info(_Info, State) ->
    {ok, State}.
