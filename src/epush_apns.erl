-module(epush_apns).

-export([handle_http/2,
         loop/4, handle_info/2
        ]).

-include_lib("eutil/include/eutil.hrl").


handle_push(ApnsName, Headers, #{<<"push_method">> := <<"general_notification">>,
                                 <<"device_token">> := DeviceToken,
                                 <<"title">> := _Title, <<"content">> := Content}) ->
    Notification = #{aps => #{alert => Content, badge => 1}},
    do_push(ApnsName, Headers, DeviceToken, Notification);

handle_push(ApnsName, Headers, #{<<"push_method">> := <<"general_app_msg">>,
                                 <<"device_token">> := DeviceToken, <<"msg">> := Msg}) ->
    Notification = #{aps => #{alert => Msg, badge => 1}},
    do_push(ApnsName, Headers, DeviceToken, Notification);

handle_push(ApnsName, Headers, #{<<"token">> := DeviceToken, <<"content">> := Content}) ->
    Notification = #{aps => #{alert => Content, badge => 1}},
    do_push(ApnsName, Headers, DeviceToken, Notification).


loop(_RoutingKey, _ContentType, Payload, #{apns_name := ApnsName, headers := Headers} = State) ->
    case handle_push(ApnsName, Headers, eutil:json_decode(Payload)) of
        reject -> {reject, State};
        _ -> {ack, State}
    end.

handle_info(Info, State) ->
    ?INFO_MSG("*******Info: ~p~n", [Info]),
    {ok, State}.

handle_http(#{id := ApnsName, headers := Headers}, Payload) ->
    handle_push(ApnsName, Headers, Payload).


do_push(ApnsName, Headers, DeviceToken, Notification) ->
    try
        case apns:push_notification(ApnsName, DeviceToken, Notification, Headers) of
            {200, _, _} ->
                ok;
            {Code, Info, Reason} ->
                ?ERROR_MSG("apns push error, Code: ~p, DeviceToken: ~p, Info: ~p, Reason: ~p",
                           [Code, DeviceToken, Info, Reason]),
                {error, Reason}
        end
    catch
        exit:{timeout, _} ->
            ?ERROR_MSG("apns push timeout, DeviceToken: ~p", [DeviceToken]),
            reject;
        Error:ErrReason ->
            ?ERROR_MSG("Error: ~p, ErrReason: ~p", [Error, ErrReason]),
            {error, ErrReason}
    end.
