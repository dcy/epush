-module(epush_apns).

-export([handle_http/2,
         loop/4, handle_info/2,
         handle_apns_error/2, handle_apns_delete_subscription/1
        ]).

-include_lib("apns/include/apns.hrl").
-include_lib("eutil/include/eutil.hrl").

handle_apns_error(MsgId, Status) ->
    ?ERROR_MSG("epush_apns error: ~p - ~p", [MsgId, Status]).

handle_apns_delete_subscription(Data) ->
    ?INFO_MSG("epush_apns delete subscription: ~p", [Data]).

handle_push(ApnsName, #{<<"token">> := DeviceToken, <<"content">> := Content}) ->
    apns:send_message(ApnsName, #apns_msg{alert = Content, badge = 1, sound = "default",
                                          device_token = binary_to_list(DeviceToken)});

handle_push(ApnsName, Payload) ->
    ApnsMsg = format_apns_msg(Payload),
    apns:send_message(ApnsName, ApnsMsg).



loop(_RoutingKey, _ContentType, Payload, #{apns_name:=ApnsName} = State) ->
    handle_push(ApnsName, jiffy:decode(Payload, [return_maps])),
    {ack, State}.

handle_info(Info, State) ->
    ?INFO_MSG("*******Info: ~p~n", [Info]),
    {ok, State}.

format_apns_msg(#{<<"device_token">> := DeviceToken} = Payload) ->
    InitMsg = #apns_msg{},
    IdMsg = case maps:get(<<"id">>, Payload, undefined) of
                undefined -> InitMsg;
                Id -> InitMsg#apns_msg{id = binary_to_list(Id)}
            end,
    ExpiryMsg = case maps:get(<<"expiry">>, Payload, undefined) of
                    undefined -> IdMsg;
                    Expiry -> IdMsg#apns_msg{expiry = binary_to_list(Expiry)}
                end,
    AlertMsg = case maps:get(<<"alert">>, Payload, undefined) of
                   undefined -> InitMsg;
                   Alert -> ExpiryMsg#apns_msg{alert = binary_to_list(Alert)}
               end,
    BadgeMsg = case maps:get(<<"badge">>, Payload, undefined) of
                   undefined -> AlertMsg;
                   Badge -> AlertMsg#apns_msg{badge = Badge}
               end,
    CategoryMsg = case maps:get(<<"category">>, Payload, undefined) of
                      undefined -> BadgeMsg;
                      Category -> BadgeMsg#apns_msg{category = binary_to_list(Category)}
                  end,
    SoundMsg = case maps:get(<<"sound">>, Payload, undefined) of
                   undefined -> BadgeMsg;
                   Sound -> CategoryMsg#apns_msg{sound = binary_to_list(Sound)}
               end,
    PriorityMsg = case maps:get(<<"priority">>, Payload, undefined) of
                      undefined -> SoundMsg;
                      Priority -> SoundMsg#apns_msg{priority = Priority}
                  end,
    case maps:get(<<"content">>, Payload, undefined) of
        undefined -> PriorityMsg#apns_msg{device_token = binary_to_list(DeviceToken)};
        Content -> PriorityMsg#apns_msg{device_token = binary_to_list(DeviceToken),
                                        apns_extra = [{content, Content}]}
    end.

handle_http(#{id := ApnsName}, Payload) ->
    handle_push(ApnsName, Payload).
