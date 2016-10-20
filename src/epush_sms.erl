-module(epush_sms).
-export([handle_send/2,
         loop/4, handle_info/2
        ]).
-include("epush.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

handle_send(MQPayload, #{sms_type:=yunpian} = State) ->
    handle_send(yunpian, MQPayload, State).

handle_send(yunpian, MQPayload, #{apikey:=Apikey}) ->
    #{<<"type">> := Type, <<"mobile">> := Mobile, <<"content">> := Content} = jiffy:decode(MQPayload, [return_maps]),
    yunpian_send(Type, Apikey, Mobile, Content).

yunpian_send(<<"single">>, Apikey, Mobile, Content) ->
    eyunpian_sms:single_send(Apikey, Mobile, Content);
yunpian_send(<<"batch">>, Apikey, Mobile, Content) ->
    eyunpian_sms:batch_send(Apikey, Mobile, Content).



loop(_RoutingKey, _ContentType, Payload, State) ->
    %% todo: 很那抉择哪种情况要放回队列，哪种情况直接略过
    %% yunpian 那边返回码很多情况
    case handle_send(Payload, State) of
        ok -> {ack, State};
        error -> {ack, State};
        {error, _} -> {ack, State}
    end.

handle_info(_Info, State) ->
    {ok, State}.
