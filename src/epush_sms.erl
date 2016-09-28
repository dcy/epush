-module(epush_sms).
-export([handle_send/2,
         loop/4, handle_info/2,
         single_test/0, batch_test/0
        ]).
-include("epush.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

handle_send(MQPayload, #{sms_type:=yunpian} = State) ->
    handle_send(yunpian, MQPayload, State).

handle_send(yunpian, MQPayload, #{apikey:=Apikey}) ->
    #{<<"type">> := Type, <<"mobile">> := Mobile, <<"content">> := Content} = jiffy:decode(MQPayload, [return_maps]),
    yunpian_send(Type, Apikey, Mobile, Content).

yunpian_send(<<"single">>, Apikey, Mobile, Content) ->
    Datas = #{apikey => Apikey, mobile => Mobile, text => Content},
    Payload = epush_util:urlencode(Datas),
    URL = <<"http://sms.yunpian.com/v2/sms/single_send.json">>,
    Method = post,
    Options = [{pool, sms}],
    Headers = [{<<"Content-Type">>, <<"application/x-www-form-urlencoded; charset=utf-8">>}],
    {ok, _StatusCode, _RespHeaders, ClientRef} = hackney:request(Method, URL, Headers,
                                                                 Payload, Options),
    {ok, ResultBin} = hackney:body(ClientRef),
    Result = jiffy:decode(ResultBin, [return_maps]),
    case maps:get(<<"code">>, Result) of
        0 ->
            ok;
        _ ->
            ?ERROR_MSG("epush sms yunpian single_send error, Mobile: ~p, Result: ~p", [Mobile, Result]),
            error
    end;
yunpian_send(<<"batch">>, Apikey, Mobile, Content) ->
    Datas = #{apikey => Apikey, mobile => Mobile, text => Content},
    Payload = epush_util:urlencode(Datas),
    URL = <<"http://sms.yunpian.com/v2/sms/batch_send.json">>,
    Method = post,
    Options = [{pool, sms}],
    Headers = [{<<"Content-Type">>, <<"application/x-www-form-urlencoded; charset=utf-8">>}],
    {ok, _StatusCode, _RespHeaders, ClientRef} = hackney:request(Method, URL, Headers,
                                                                 Payload, Options),
    {ok, ResultBin} = hackney:body(ClientRef),
    Result = jiffy:decode(ResultBin, [return_maps]),
    #{<<"total_count">> := TotalCount} = Result,
    case TotalCount of
        0 ->
            ?ERROR_MSG("epush sms yunpian batch_send no success, Mobile: ~p", [Mobile]),
            ok;
        _ ->
            ok
    end.




loop(_RoutingKey, _ContentType, Payload, State) ->
    %% todo: 很那抉择哪种情况要放回队列，哪种情况直接略过
    %% yunpian 那边返回码很多情况
    case handle_send(Payload, State) of
        ok -> {ack, State};
        error -> {ack, State}
    end.

handle_info(_Info, State) ->
    {ok, State}.







single_test() ->
    {ok, Connection} =
    amqp_connection:start(#amqp_params_network{host = "localhost"}),
    {ok, Channel} = amqp_connection:open_channel(Connection),

    Mobile = "15102025006",
    Content = unicode:characters_to_binary("【广州灵光科技】验证码6666，请您尽快验证，完成Hisir注册。如非本人操作请忽略。"),

    amqp_channel:cast(Channel,
                      #'basic.publish'{exchange = <<"">>,
                                       routing_key = <<"yunpian">>},
                      #amqp_msg{payload = jiffy:encode(#{mobile=>Mobile, content=>Content, type=>"single"})}),
    amqp_channel:close(Channel),
    amqp_connection:close(Connection),
    ok.

batch_test() ->
    {ok, Connection} =
    amqp_connection:start(#amqp_params_network{host = "localhost"}),
    {ok, Channel} = amqp_connection:open_channel(Connection),

	Mobile = "15102025006,15013276061",
    Content = unicode:characters_to_binary("【广州灵光科技】验证码8666，请您尽快验证，完成Hisir注册。如非本人操作请忽略。"),

    amqp_channel:cast(Channel,
                      #'basic.publish'{exchange = <<"">>,
                                       routing_key = <<"yunpian">>},
                      #amqp_msg{payload = jiffy:encode(#{mobile=>Mobile, content=>Content, type=>"batch"})}),
    amqp_channel:close(Channel),
    amqp_connection:close(Connection),
    ok.


