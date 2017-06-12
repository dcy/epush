-module(epush_yunpian).
-export([handle_http/2,
         loop/4, handle_info/2
        ]).

-include_lib("eutil/include/eutil.hrl").

handle_push(Apikey, #{<<"type">> := <<"single">>, <<"mobile">> := Mobile,
                      <<"content">> := Content}) ->
    ?INFO_MSG("epush_yunpian, Mobile: ~p", [Mobile]),
    eyunpian_sms:single_send(Apikey, Mobile, Content);

handle_push(Apikey, #{<<"type">> := <<"batch">>, <<"mobile">> := Mobile,
                      <<"content">> := Content}) ->
    eyunpian_sms:batch_send(Apikey, Mobile, Content).

loop(_RoutingKey, _ContentType, BinPayload, State) ->
    %% todo: 抉择哪种情况要放回队列，哪种情况直接略过
    %% yunpian 那边返回码很多情况
    Payload = eutil:json_decode(BinPayload),
    case handle_push(maps:get(apikey, State), Payload) of
        {ok, _} ->
            {ack, State};
        {error, ErrResult} ->
            #{<<"mobile">> := Mobile, <<"content">> := Content} = Payload,
            case maps:get(<<"code">>, ErrResult) of
                -50 -> handle_backup(Mobile, Content);
                -51 -> handle_backup(Mobile, Content);
                _ -> ignore
            end,
            spawn(fun() -> handle_fail_warn(Mobile, ErrResult) end),
            {ack, State}
    end.

handle_info(_Info, State) ->
    {ok, State}.

handle_http(#{apikey := Apikey}, Payload) ->
    handle_push(Apikey, Payload).


handle_backup(Mobile, Content) ->
    case application:get_env(epush, sms_fail_conf) of
        undefined ->
            ignore;
        {ok, Conf} ->
            case maps:get(backup, Conf, undefined) of
                undefined ->
                    ignore;
                #{sdkappid := Sdkappid, appkey := Appkey} ->
                    eqsms:send_isms(Sdkappid, Appkey, Mobile, Content)
            end
    end.
                    

handle_fail_warn(Mobile, ErrResult) ->
    case application:get_env(epush, sms_fail_conf) of
        undefined ->
            ignore;
        {ok, Conf} ->
            case maps:get(sms_warn, Conf, undefined) of
                undefined -> ignore;
                SmsConf -> handle_sms_warn(Mobile, ErrResult, SmsConf)
            end,
            case maps:get(email_warn, Conf, undefined) of
                undefined -> ignore;
                EmailConf -> handle_email_warn(Mobile, ErrResult, EmailConf)
            end
    end.



handle_email_warn(Mobile, ErrResult, EmailConf) ->
    #{receivers := Receivers, opts := Opts} = EmailConf,
    Sender = proplists:get_value(username, Opts),
    Body = gen_email_body(Mobile, Sender, ErrResult),
    case gen_smtp_client:send_blocking({Sender, Receivers, Body}, Opts) of
        {error, ErrReason} ->
            ?ERROR_MSG("send mail warn error, Mobile: ~p, ErrReason: ~p", [Mobile, ErrReason]);
        {error, ErrReason1, ErrReason2} ->
            ?ERROR_MSG("send mail warn error, Mobile: ~p, ErrReason1: ~p, ErrReason2: ~p", [Mobile, ErrReason1, ErrReason2]);
        _ ->
            ignore
    end.

gen_email_body(Mobile, From, ErrResult) ->
    Code = maps:get(<<"code">>, ErrResult),
    Subject = <<(unicode:characters_to_binary("HisirEpush云片发送失败code: "))/binary, (erlang:integer_to_binary(Code))/binary>>,
    MobileBody = <<(unicode:characters_to_binary("电话号码: "))/binary, Mobile/binary, "\r\n">>,
    DetailBody = case maps:get(<<"detail">>, ErrResult, undefined) of
                     undefined -> <<(unicode:characters_to_binary("详情: "))/binary, "No Detail">>;
                     Detail -> <<(unicode:characters_to_binary("详情: "))/binary, Detail/binary>>
                 end,
    EmailBody = <<MobileBody/binary, DetailBody/binary>>,

    mimemail:encode({<<"text">>, <<"plain">>,
                     [{<<"Subject">>, Subject},
                      {<<"From">>, eutil:to_binary(From)}],
                     [],
                     EmailBody}).

handle_sms_warn(Mobile, ErrResult, SmsConf) ->
    #{receivers := Receivers, sms_push_id := SmsPushId} = SmsConf,
    case Receivers of
        [] ->
            ignore;
        _ ->
            case epush_util:get_push_conf(eutil:to_binary(SmsPushId)) of
                undefined ->
                    ignore;
                #{apikey := Apikey} ->
                    NewReceivers = list_to_binary(string:join(Receivers, ",")),
                    #{<<"code">> := Code, <<"detail">> := Detail} = ErrResult,
                    %云片短信发送失败，号码：#phone#，API返回码：#code#，详情：#detail#
                    Content = <<(unicode:characters_to_binary("云片短信发送失败，号码："))/binary, (eutil:to_binary(Mobile))/binary, (unicode:characters_to_binary("，API返回码："))/binary, (eutil:to_binary(Code))/binary, (unicode:characters_to_binary("，详情："))/binary, Detail/binary>>,
                    case eyunpian_sms:batch_send(Apikey, NewReceivers, Content) of
                        {ok, _} -> ignore;
                        {error, Reason} -> ?ERROR_MSG("handle_sms_warn, Receivers: ~p, Reason: ~p", [Receivers, Reason])
                    end
            end
    end.

        
