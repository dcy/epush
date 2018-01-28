-module(epush_apns).

-export([handle_http/2,
         loop/4, handle_info/2
        ]).

-include_lib("eutil/include/eutil.hrl").


handle_push(ApnsName, Timeout, Headers, #{<<"push_method">> := <<"general_notification">>,
                                 <<"device_token">> := DeviceToken,
                                 <<"title">> := _Title, <<"content">> := Content} = Payload) ->
    Aps = #{alert => Content, badge => 1},
    Sound = maps:get(<<"sound">>, Payload, <<"default">>),
    SoundAps = Aps#{sound => Sound},
    Notification = #{aps => SoundAps},
    do_push(ApnsName, Timeout, Headers, DeviceToken, Notification);

handle_push(ApnsName, Timeout, Headers, #{<<"push_method">> := <<"general_app_msg">>,
                                 <<"device_token">> := DeviceToken, <<"msg">> := Msg}) ->
    Notification = #{aps => #{alert => Msg, badge => 1}},
    do_push(ApnsName, Timeout, Headers, DeviceToken, Notification);

handle_push(ApnsName, Timeout, Headers, #{<<"token">> := DeviceToken, <<"content">> := Content}) ->
    Notification = #{aps => #{alert => Content, badge => 1}},
    do_push(ApnsName, Timeout, Headers, DeviceToken, Notification).


loop(_RoutingKey, _ContentType, Payload, #{pool_name := PoolName, headers := Headers,
                                           timeout := Timeout} = State) ->
    case handle_push(PoolName, Timeout, Headers, eutil:json_decode(Payload)) of
        reject -> {reject, State};
        _ -> {ack, State}
    end.

handle_info(Info, State) ->
    ?INFO_MSG("*******Info: ~p~n", [Info]),
    {ok, State}.

handle_http(#{id := ApnsName, headers := Headers, timeout := Timeout}, Payload) ->
    PoolName = list_to_atom(lists:concat([ApnsName, "_pool"])),
    handle_push(PoolName, Timeout,  Headers, Payload).


%do_push(ApnsName, Headers, DeviceToken, Notification) ->
%    try
%        case apns:push_notification(ApnsName, DeviceToken, Notification, Headers) of
%            {200, _, _} ->
%                ok;
%            {Code, Info, Reason} ->
%                ?ERROR_MSG("apns push error, Code: ~p, DeviceToken: ~p, Info: ~p, Reason: ~p",
%                           [Code, DeviceToken, Info, Reason]),
%                {error, Reason}
%        end
%    catch
%        exit:{timeout, _} ->
%            ?ERROR_MSG("apns push timeout, DeviceToken: ~p", [DeviceToken]),
%            reject;
%        Error:ErrReason ->
%            ?ERROR_MSG("Error: ~p, ErrReason: ~p", [Error, ErrReason]),
%            {error, ErrReason}
%    end.
do_push(ApnsName, Timeout, Headers, DeviceToken, Notification) ->
    case epush_pool:push(ApnsName, DeviceToken, Notification, Headers, Timeout+1000) of
        {200, _, _} ->
            ok;
        {timeout, _} ->
            ?ERROR_MSG("apns do_push timeout, DeviceToken: ~p", [DeviceToken]),
            reject;
        {Code, Info, Reason} ->
            ?ERROR_MSG("apns do_push error, Code: ~p, DeviceToken: ~p, Info: ~p, Reason: ~p",
                       [Code, DeviceToken, Info, Reason]),
            {error, Reason};
        Error ->
            ?ERROR_MSG("apns do_push error, Error: ~p", [Error])
    end.
