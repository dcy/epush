-module(epush_fcm).
-export([handle_send/2,
         loop/4, handle_info/2
        ]).
-include("epush.hrl").

-define(URL, <<"https://fcm.googleapis.com/fcm/send">>).

format_multi_topics(Topics) ->
    Fun = fun(Topic) ->
                  binary_to_list(<<"'", Topic/binary, "' in topics">>)
          end,
    TopicList = lists:map(Fun, Topics),
    string:join(TopicList, " || ").


handle_send(MQPayload, #{headers:=Headers, proxy:=Proxy}) ->
    PayloadMaps = jiffy:decode(MQPayload, [return_maps]),
    case maps:get(<<"push_method">>, PayloadMaps, undefined) of
        <<"notification">> ->
            #{<<"title">> := Title, <<"content">> := Body, <<"to">> := To} = PayloadMaps,
            Notification = #{<<"title">> => Title, <<"body">> => Body, <<"icon">> => <<"Hisir">>},
            Msg = #{<<"to">> => To, <<"notification">> => Notification},
            do_send(Msg, Headers, Proxy);
        <<"unvarnished">> ->
            #{<<"to">> := To, <<"content">> := Content} = PayloadMaps,
            Data = #{<<"content">> => Content},
            Msg = #{<<"to">> => To, <<"data">> => Data},
            do_send(Msg, Headers, Proxy);
        <<"topics">> ->
            #{<<"topics">> := Topics, <<"content">> := Content} = PayloadMaps,
            case Topics of
                [Topic] ->%%单主题
                    Msg = #{<<"to">> => Topic, <<"data">> => Content},
                    do_send(Msg, Headers, Proxy);
                _ -> %%多主题
                    Condition = format_multi_topics(Topics),
                    Msg = #{<<"condition">> => Condition, <<"data">> => Content},
                    do_send(Msg, Headers, Proxy)
            end;
        undefined ->
            #{<<"token">> := Token, <<"content">> := Content} = PayloadMaps,
            Msg = #{to => Token, data => #{content => Content}},
            do_send(Msg, Headers, Proxy)
    end.

do_send(PayloadMaps, Headers, Proxy) ->
    Method = post,
    Payload = jiffy:encode(PayloadMaps),
    Options = case Proxy of
                  undefined ->[{pool, fcm}];
                  _ -> [{pool, default}, {proxy, Proxy}]
              end,
    {ok, StatusCode, _RespHeaders, ClientRef} = hackney:request(Method, ?URL, Headers,
                                                                Payload, Options),
    case StatusCode of
        200 ->
            {ok, ResultBin} = hackney:body(ClientRef),
            Result = jiffy:decode(ResultBin, [return_maps]),
            case maps:get(<<"success">>, Result) of
                1 ->
                    ok;
                0 ->
                    ?ERROR_MSG("epush fcm error, PayloadMaps:~p, Result: ~p", [PayloadMaps, Result]),
                    ok 
            end;
        _ ->
            ?ERROR_MSG("epush fcm error, StatusCode: ~p, PayloadMaps: ~p", [StatusCode, PayloadMaps]),
            ok
    end.

loop(_RoutingKey, _ContentType, Payload, State) ->
    handle_send(Payload, State),
    {ack, State}.

handle_info(_Info, State) ->
    {ok, State}.
