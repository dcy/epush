-module(epush_worker).

-behaviour(gen_server).

%% API
-export([start_link/2]).  
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("apns/include/apns.hrl").
-include("epush.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Conf, Index) ->
    Type = maps:get(type, Conf),
    ProcName = gen_proc_name(Type, Index),
    gen_server:start_link({local, ProcName}, ?MODULE, [Type, Conf], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Type, Conf]) ->
    {ok, init_worker(Type, Conf)}.
%{ok, #state{type=maps:get(name, PushConf), channel=Channel, connection=Connection}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(refresh_access_token, State) ->
    erlang:send_after(86400000, self(), refresh_access_token),
    #{app_id := AppId, app_secret := AppSecret} = State,
    AccessToken = epush_huawei:get_access_token(AppId, AppSecret),
    {noreply, State#{access_token => AccessToken}};

handle_info(#'basic.consume_ok'{}, State) ->
    {noreply, State};

handle_info({#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload = Payload}}, #{type:=apns} = State) ->
    epush_apns:handle_send(Payload),
    amqp_channel:cast(maps:get(channel, State), #'basic.ack'{delivery_tag = Tag}),
    {noreply, State};

handle_info({#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload = Payload}}, #{type:=xiaomi} = State) ->
    case epush_xiaomi:handle_send(Payload, State) of
        ok -> amqp_channel:cast(maps:get(channel, State), #'basic.ack'{delivery_tag = Tag});
        _ -> do_nothing
    end,
    {noreply, State};

handle_info({#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload = Payload}}, #{type:=huawei} = State) ->
    case epush_huawei:handle_send(Payload, State) of
        ok -> amqp_channel:cast(maps:get(channel, State), #'basic.ack'{delivery_tag = Tag});
        _ -> do_nothing
    end,
    {noreply, State};

handle_info({#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload = Payload}}, #{type:=fcm} = State) ->
    case epush_fcm:handle_send(Payload, State) of
        ok -> amqp_channel:cast(maps:get(channel, State), #'basic.ack'{delivery_tag = Tag});
        _ -> do_nothing
    end,
    {noreply, State};

handle_info({#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload = Payload}}, #{type:=sms} = State) ->
    case epush_sms:handle_send(Payload, State) of
        ok -> amqp_channel:cast(maps:get(channel, State), #'basic.ack'{delivery_tag = Tag});
        _ -> do_nothing
    end,
    {noreply, State};

handle_info(Info, State) ->
    lager:info("******handle_info, Info: ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #{channel:=Channel, connection:=Connection}) ->
    amqp_channel:close(Channel),
    amqp_connection:close(Connection),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
gen_proc_name(Type, Index) ->
    erlang:list_to_atom(lists:concat(["epush_worker_", Type, "_", Index])).

init_worker(apns, Conf) ->
    #{rmq_host := Host, rmq_user := Username, rmq_passwd := Password} = Conf,
    TypeBin = erlang:atom_to_binary(apns, utf8),
    {ok, Connection} =
    amqp_connection:start(#amqp_params_network{host = Host,
                                               username = list_to_binary(Username),
                                               password = list_to_binary(Password)}),
    {ok, Channel} = amqp_connection:open_channel(Connection),

    amqp_channel:call(Channel, #'queue.declare'{queue = TypeBin,
                                                durable = true}),

    amqp_channel:call(Channel, #'basic.qos'{prefetch_count = 1}),
    amqp_channel:subscribe(Channel, #'basic.consume'{queue = TypeBin}, self()),
    #{type=>apns, channel=>Channel, connection=>Connection};

init_worker(xiaomi, Conf) ->
    #{rmq_host := Host, pkg_name := PkgName, app_secret := AppSecret,
      rmq_user := Username, rmq_passwd := Password} = Conf,
    TypeBin = <<"xiaomi">>,
    {ok, Connection} =
    amqp_connection:start(#amqp_params_network{host = Host,
                                               username = list_to_binary(Username),
                                               password = list_to_binary(Password)}),
    {ok, Channel} = amqp_connection:open_channel(Connection),

    amqp_channel:call(Channel, #'queue.declare'{queue = TypeBin,
                                                durable = true}),

    amqp_channel:call(Channel, #'basic.qos'{prefetch_count = 1}),
    amqp_channel:subscribe(Channel, #'basic.consume'{queue = TypeBin}, self()),
    Headers = [{<<"Content-Type">>, <<"application/x-www-form-urlencoded; charset=utf-8">>},
               {<<"Authorization">>, list_to_binary(lists:concat(["key=", AppSecret]))}],
    #{type=>xiaomi, pkg_name=>PkgName, app_secret=>AppSecret, headers=>Headers,
      channel=>Channel, connection=>Connection};

init_worker(huawei, Conf) ->
    #{rmq_host := Host, app_id := AppId, app_secret := AppSecret,
      rmq_user := Username, rmq_passwd := Password} = Conf,
    AccessToken = epush_huawei:get_access_token(AppId, AppSecret),
    erlang:send_after(86400000, self(), refresh_access_token),
    TypeBin = <<"huawei">>,
    {ok, Connection} =
    amqp_connection:start(#amqp_params_network{host = Host,
                                               username = list_to_binary(Username),
                                               password = list_to_binary(Password)}),
    {ok, Channel} = amqp_connection:open_channel(Connection),

    amqp_channel:call(Channel, #'queue.declare'{queue = TypeBin,
                                                durable = true}),

    amqp_channel:call(Channel, #'basic.qos'{prefetch_count = 1}),
    amqp_channel:subscribe(Channel, #'basic.consume'{queue = TypeBin}, self()),
    #{type=>huawei, app_id=>AppId, app_secret=>AppSecret, access_token=>AccessToken,
      channel=>Channel, connection=>Connection};

init_worker(fcm, Conf) ->
    #{rmq_host := Host, app_secret := AppSecret, proxy := Proxy,
      rmq_user := Username, rmq_passwd := Password} = Conf,
    TypeBin = <<"fcm">>,
    {ok, Connection} =
    amqp_connection:start(#amqp_params_network{host = Host,
                                               username = list_to_binary(Username),
                                               password = list_to_binary(Password)}),
    {ok, Channel} = amqp_connection:open_channel(Connection),

    amqp_channel:call(Channel, #'queue.declare'{queue = TypeBin,
                                                durable = true}),

    amqp_channel:call(Channel, #'basic.qos'{prefetch_count = 1}),
    amqp_channel:subscribe(Channel, #'basic.consume'{queue = TypeBin}, self()),
    Headers = [{<<"Content-Type">>, <<"application/json; charset=utf-8">>},
               {<<"Authorization">>, list_to_binary(lists:concat(["key=", AppSecret]))}],
    #{type=>fcm, app_secret=>AppSecret, headers=>Headers, proxy=>Proxy,
      channel=>Channel, connection=>Connection};

init_worker(sms, Conf) ->
    #{sms_type := SmsType, apikey := Apikey, rmq_host := Host,
      rmq_user := Username, rmq_passwd := Password} = Conf,
    TypeBin = <<"sms">>,
    {ok, Connection} =
    amqp_connection:start(#amqp_params_network{host = Host,
                                               username = list_to_binary(Username),
                                               password = list_to_binary(Password)}),
    {ok, Channel} = amqp_connection:open_channel(Connection),

    amqp_channel:call(Channel, #'queue.declare'{queue = TypeBin,
                                                durable = true}),

    amqp_channel:call(Channel, #'basic.qos'{prefetch_count = 1}),
    amqp_channel:subscribe(Channel, #'basic.consume'{queue = TypeBin}, self()),
    #{type=>sms, sms_type=>SmsType, apikey=>Apikey,
      channel=>Channel, connection=>Connection}.
