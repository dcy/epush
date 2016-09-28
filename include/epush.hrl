-ifndef(__EPUSH_HRL__).
-define(__EPUSH_HRL__, 0).

-define(DEBUG_MSG(Str), lager:debug(Str)).
-define(DEBUG_MSG(Format, Args), lager:debug(Format, Args)).
-define(INFO_MSG(Str), lager:info(Str)).
-define(INFO_MSG(Format, Args), lager:info(Format, Args)).
-define(NOTICE_MSG(Str), lager:notice(Str)).
-define(NOTICE_MSG(Format, Args), lager:notice(Format, Args)).
-define(WARNING(Str), lager:warning(Str)).
-define(WARNING_MSG(Format, Args), lager:warning(Format, Args)).
-define(ERROR_MSG(Str), lager:error(Str)).
-define(ERROR_MSG(Format, Args), lager:error(Format, Args)).
-define(CRITICAL_MSG(Str), lager:critical(Str)).
-define(CRITICAL_MSG(Format, Args), lager:critical(Format, Args)).
-define(ALERT_MSG(Str), lager:alert(Str)).
-define(ALERT_MSG(Format, Args), lager:alert(Format, Args)).


-define(TRY_CATCH(Expression), ?TRY_CATCH(Expression, ErrReason)).
-define(TRY_CATCH(Expression, ErrReason), try
                                              Expression
                                          catch 
                                              _:ErrReason ->
                                                  ?ERROR_MSG("ErrReason:~p, Stacktrace:~p", [ErrReason, erlang:get_stacktrace()])
                                          end).

-define(ASSERT(BoolExpr), ((fun() ->
                                    case (BoolExpr) of
                                        true -> void;
                                        __V -> erlang:error({assert_failed,
                                                             [{module, ?MODULE},
                                                              {line, ?LINE},
                                                              {expression, (??BoolExpr)},
                                                              {expected, true},
                                                              {value, case __V of
                                                                          false -> __V;
                                                                          _ -> {not_a_boolean, __V}
                                                                      end}]
                                                            })
                                    end
                            end)())).
								
-define(ASSERT(BoolExpr, Msg), ((fun() ->
                                         case (BoolExpr) of
                                             true -> void;
                                             _V -> erlang:error(Msg)
                                         end
                                 end)())).

-define(ENSURE(BoolExpr, RetCode), ((fun() -> case (BoolExpr) of
                                                  true -> ok;
                                                  false -> erlang:throw(RetCode)
                                              end
                                     end)())).

-define(TRACE(Str), io:format(Str)).
-define(TRACE(Str, Args), io:format(Str, Args)).

-define(TRACE_VAR(Arg), io:format("~n******~nModule: ~p, Line: ~p, ~nMy print's ~p is ~p~n******~n", [?MODULE, ?LINE, ??Arg, Arg])).

-define(HANDLE_CALL(Request, From, State),
        try
            handle_call_(Request, From, State)
        catch
            Type:Reason ->
                ?ERROR_MSG("~p call error, Request:~p, Type:~p, Reason:~p, StackTrace:~p",
                           [?MODULE, Request, Type, Reason, erlang:get_stacktrace()]),
                {reply, {error, Type, Reason}, State}
        end
       ).

-define(HANDLE_CAST(Msg, State),
        try
            handle_cast_(Msg, State)
        catch
            Type:Reason ->
                ?ERROR_MSG("~p cast error, Msg:~p, Type:~p, Reason:~p, StackTrace:~p",
                           [?MODULE, Msg, Type, Reason, erlang:get_stacktrace()]),
                {noreply, State}
        end
       ).

-define(HANDLE_INFO(Info, State),
        try
            handle_info_(Info, State)
        catch
            Type:Reason ->
                ?ERROR_MSG("~p info error, Info:~p, Type:~p, Reason:~p, StackTrace:~p",
                           [?MODULE, Info, Type, Reason, erlang:get_stacktrace()]),
                {noreply, State}
        end
       ).

-define(LOG_NOT_HANDLE_CALL(Request),
        ?CRITICAL_MSG("~p not handle call error, Request:~p, Stacktrace:~p",
                      [?MODULE, Request, erlang:get_stacktrace()])
       ).

-define(LOG_NOT_HANDLE_CAST(Msg),
        ?CRITICAL_MSG("~p not handle cast error, Msg:~p, Stacktrace:~p",
                      [?MODULE, Msg, erlang:get_stacktrace()])
       ).

-define(LOG_NOT_HANDLE_INFO(Info),
        ?CRITICAL_MSG("~p not handle info error, Info:~p, Stacktrace:~p",
                      [?MODULE, Info, erlang:get_stacktrace()])
       ).

-define(LOG_NOT_HANDLE_INFO(Info, State),
        ?CRITICAL_MSG("~p not handle info error, Info:~p, State:~p, Stacktrace:~p",
                      [?MODULE, Info, State, erlang:get_stacktrace()])
       ).




-endif.
