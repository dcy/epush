%%%-------------------------------------------------------------------
%% @doc epush public API
%% @end
%%%-------------------------------------------------------------------

-module(epush_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-include("epush.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("apns/include/apns.hrl").

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    case epush_sup:start_link() of
        {error, _} = E ->
            E;
        R ->
            init(),
            R
    end.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
init() ->
    epush_statistics:start_link(),
    epush_apns:init(),
    start_workers(),
    ok.

start_workers() ->
    {ok, PushConfs} = application:get_env(epush, push_confs),
    [start_worker(Conf) || Conf <- PushConfs],
    ok.

start_worker(#{type:=apns, pool_size:=PoolSize, queue:=Queue, cert_file:=CertFile,
               is_sandbox_env:=IsSandbox}) ->
    {AppleHost, FeedbackHost} = case IsSandbox of
                                    true -> {"gateway.sandbox.push.apple.com", "feedback.sandbox.push.apple.com"};
                                    false -> {"gateway.push.apple.com", "feedback.push.apple.com"}
                                end,
    DefConn = apns:default_connection(),
    ApnsConn = DefConn#apns_connection{cert_file = CertFile, apple_host = AppleHost,
                                       feedback_host = FeedbackHost},
    ApnsName = binary_to_atom(Queue, utf8),
    apns:connect(ApnsName, ApnsConn),
    gen_subscriber(apns, Queue, PoolSize, #{type=>apns, apns_name=>ApnsName});
start_worker(#{type:=xiaomi, pkg_name:=PkgName, app_secret:=AppSecret, pool_size:=PoolSize,
               queue:=Queue}) ->
    Headers = [{<<"Content-Type">>, <<"application/x-www-form-urlencoded; charset=utf-8">>},
               {<<"Authorization">>, list_to_binary(lists:concat(["key=", AppSecret]))}],
    State = #{type=>xiaomi, pkg_name=>PkgName, app_secret=>AppSecret, headers=>Headers},
    {ok, _} = gen_subscriber(xiaomi, Queue, PoolSize, State);
start_worker(#{type:=huawei, app_id:=AppId, app_secret:=AppSecret, pool_size:=PoolSize,
               queue:=Queue}) ->
    State = #{type=>huawei, app_id=>AppId, app_secret=>AppSecret},
    {ok, _Pid} = gen_subscriber(huawei, Queue, PoolSize, State),
    %Pid = gproc:where({n,l,{turtle,service,huawei_subscriber}}),
    %ChildSpecs = supervisor:which_children(Pid),
    %PoolSpec = lists:keyfind(pool, 1, ChildSpecs),
    %{pool, PoolPid, supervisor, [turtle_subscriber_pool]} = PoolSpec,
    %SubsriberSpecs = supervisor:which_children(PoolPid),
    %Fun = fun({_, SubsriberPid, worker, [turtle_subscriber]}) ->
    %              erlang:send_after(6000, SubsriberPid, refresh_access_token)
    %      end,
    %lists:foreach(Fun, SubsriberSpecs),
    ok;
start_worker(#{type:=fcm, app_secret:=AppSecret, proxy:=Proxy, pool_size:=PoolSize,
               queue:=Queue}) ->
    Headers = [{<<"Content-Type">>, <<"application/json; charset=utf-8">>},
               {<<"Authorization">>, list_to_binary(lists:concat(["key=", AppSecret]))}],
    State = #{type=>fcm, app_secret=>AppSecret, headers=>Headers, proxy=>Proxy},
    {ok, _} = gen_subscriber(fcm, Queue, PoolSize, State),
    ok;
start_worker(#{type:=flyme, app_id:=AppId, app_secret:=AppSecret, pool_size:=PoolSize,
               queue:=Queue}) ->
    State = #{type=>flyme, app_id=>AppId, app_secret=>AppSecret},
    {ok, _} = gen_subscriber(flyme, Queue, PoolSize, State),
    ok;
start_worker(#{type:=sms, sms_type:=SmsType, apikey:=Apikey, pool_size:=PoolSize,
               queue:=Queue}) ->
    State = #{type=>sms, sms_type=>SmsType, apikey=>Apikey},
    {ok, _} = gen_subscriber(sms, Queue, PoolSize, State),
    ok.

gen_subscriber(Type, Queue, PoolSize, State) ->
    Mod = erlang:list_to_atom(lists:concat(["epush_", Type])),
    Declarations = [#'queue.declare'{queue = Queue, durable = true}],
    Config = #{
      name => binary_to_atom(Queue, utf8),
      connection => amqp_server,
      function => fun Mod:loop/4,
      handle_info => fun Mod:handle_info/2,
      init_state => State,
      declarations => Declarations,
      subscriber_count => PoolSize,
      prefetch_count => PoolSize,
      consume_queue => Queue,
      passive => false
     },
    ServiceSpec = turtle_service:child_spec(Config),
    supervisor:start_child(turtle_sup, ServiceSpec).

