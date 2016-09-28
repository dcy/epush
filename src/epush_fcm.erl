-module(epush_fcm).
-export([handle_send/2,
         loop/4, handle_info/2
        ]).
-include("epush.hrl").

handle_send(MQPayload, #{headers:=Headers, proxy:=Proxy}) ->
    #{<<"token">> := Token, <<"content">> := Content} = jiffy:decode(MQPayload, [return_maps]),
    Datas = #{to => Token, data => #{content => Content}},
    Method = post,
    URL = <<"https://fcm.googleapis.com/fcm/send">>,
    Payload = jiffy:encode(Datas),
    Options = case Proxy of
                  undefined ->[{pool, default}];
                  _ -> [{pool, default}, {proxy, Proxy}]
              end,
    {ok, StatusCode, _RespHeaders, ClientRef} = hackney:request(Method, URL, Headers,
                                                                Payload, Options),
    case StatusCode of
        200 ->
            {ok, ResultBin} = hackney:body(ClientRef),
            Result = jiffy:decode(ResultBin, [return_maps]),
            case maps:get(<<"success">>, Result) of
                1 ->
                    ok;
                0 ->
                    ?ERROR_MSG("epush fcm error, Result: ~p, Token: ~p", [Result, Token]),
                    error
            end;
        _ ->
            ?ERROR_MSG("epush fcm error, StatusCode: ~p, Token: ~p", [StatusCode, Token]),
            error
    end.

loop(_RoutingKey, _ContentType, Payload, State) ->
    handle_send(Payload, State),
    {ack, State}.

handle_info(_Info, State) ->
    {ok, State}.
