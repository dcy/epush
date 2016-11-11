-module(epush_apns).

-export([init/0, test/0, test1/0,
         %handle_send/2,
         handle_http/2,
         loop/4, handle_info/2,
         handle_apns_error/2, handle_apns_delete_subscription/1
        ]).

-include_lib("apns/include/apns.hrl").
-include_lib("eutil/include/eutil.hrl").

init() ->
    apns:connect(
      apns,
      fun ?MODULE:handle_apns_error/2,
      fun ?MODULE:handle_apns_delete_subscription/1
     ),
    ok.

handle_apns_error(MsgId, Status) ->
    ?ERROR_MSG("epush error: ~p - ~p", [MsgId, Status]).

handle_apns_delete_subscription(Data) ->
    ?INFO_MSG("delete subscription: ~p", [Data]).

%handle_send(ApnsName, Payload) ->
%    #{<<"token">> := DeviceToken, <<"content">> := Content} = jiffy:decode(Payload, [return_maps]),
%    apns:send_message(ApnsName, #apns_msg{alert = "alert", badge = 1, sound = "default",
%                                          device_token = binary_to_list(DeviceToken),
%                                          apns_extra = [{content, Content}]
%                                         }),
%    ok.

handle_push(ApnsName, #{<<"token">> := DeviceToken, <<"content">> := Content}) ->
    apns:send_message(ApnsName, #apns_msg{alert = "alert", badge = 1, sound = "default",
                                          device_token = binary_to_list(DeviceToken),
                                          apns_extra = [{content, Content}]
                                         });

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



test() ->
    %ipad: 639fccce4dec1eaf3f09b2fdc1a3fab7ba28f2e0703991d7caa3b29165f4f26d
    apns:send_message(apns, #apns_msg{
                               alert  = "alert" ,
                               badge  = 1,
                               sound  = "default" ,
                               category = "EMAIL_ACTION",
                               expiry = 1548000749,
                               device_token = "ae465ec6e42b5c7aeccde8ad8823e3bdca9ba29e0d48492c09e2e71ffc37ea57"
                              }).

test1() ->
    %apns:send_message(apns, "ae465ec6e42b5c7aeccde8ad8823e3bdca9ba29e0d48492c09e2e71ffc37ea57", "hello world").
    apns:send_message(apns_c, "ac18a5fa01e2dfefdf1a39eecd0316879e277d388099261fac691d58552f1126", "hello world"),
    %epush_statistics:update().
    ok.

