-module(epush_yunpian).
-export([handle_http/2,
         loop/4, handle_info/2
        ]).

-include_lib("eutil/include/eutil.hrl").

handle_push(Apikey, #{<<"type">> := <<"single">>, <<"mobile">> := Mobile,
                      <<"content">> := Content}) ->
    eyunpian_sms:single_send(Apikey, Mobile, Content);

handle_push(Apikey, #{<<"type">> := <<"batch">>, <<"mobile">> := Mobile,
                                   <<"content">> := Content}) ->
    eyunpian_sms:batch_send(Apikey, Mobile, Content).

loop(_RoutingKey, _ContentType, Payload, State) ->
    %% todo: 抉择哪种情况要放回队列，哪种情况直接略过
    %% yunpian 那边返回码很多情况
    case handle_push(maps:get(apikey, State), jiffy:decode(Payload, [return_maps])) of
        ok -> {ack, State};
        error -> {ack, State};
        {error, _} -> {ack, State}
    end.

handle_info(_Info, State) ->
    {ok, State}.

handle_http(#{apikey := Apikey}, Payload) ->
    handle_push(Apikey, Payload).
