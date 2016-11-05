-module(epush_apns).

-export([init/0, test/0, test1/0,
         handle_send/2,
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

handle_send(ApnsName, Payload) ->
    #{<<"token">> := DeviceToken, <<"content">> := Content} = jiffy:decode(Payload, [return_maps]),
    apns:send_message(ApnsName, #apns_msg{alert = "alert", badge = 1, sound = "default",
                                      device_token = binary_to_list(DeviceToken),
                                      apns_extra = [{content, Content}]
                                     }),
    ok.






loop(_RoutingKey, _ContentType, Payload, #{apns_name:=ApnsName} = State) ->
    handle_send(ApnsName, Payload),
    {ack, State}.

handle_info(Info, State) ->
    io:format("*******Info: ~p~n", [Info]),
    {ok, State}.


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
    apns:send_message(apns, "639fccce4dec1eaf3f09b2fdc1a3fab7ba28f2e0703991d7caa3b29165f4f26d", "hello world"),
    %epush_statistics:update().
    ok.

