-module(epush_pool).
-export([add_pool/2, add_pool/3,
         push/3, push/4, push/5
        ]).

-export([test_add_pool/0]).
-include_lib("eutil/include/eutil.hrl").

add_pool(PoolName, ApnsArgs) ->
    PoolArgs = [{name, {local, PoolName}}, {worker_module, epush_worker}, {size, 10}, {max_overflow, 20}],
    add_pool(PoolName, PoolArgs, ApnsArgs).

add_pool(PoolName, PoolArgs, WorkerArgs) ->
    PoolSpec = poolboy:child_spec(PoolName, PoolArgs, WorkerArgs),
    supervisor:start_child(epush_pool_sup, PoolSpec).

test_add_pool() ->
    Name = apns_pool_c,
    PoolArgs = [{name, {local, Name}}, {strategy, fifo}, {worker_module, epush_worker}, {size, 6}, {max_overflow, 10}],
    add_pool(Name, PoolArgs, []).

push(PoolName, DeviceToken, Notification) ->
    poolboy:transaction(PoolName,
                        fun(Worker) ->
                                gen_server:call(Worker, {push, DeviceToken, Notification})
                        end
                       ).

push(PoolName, DeviceToken, Notification, Timeout) when is_integer(Timeout) ->
    poolboy:transaction(PoolName,
                        fun(Worker) ->
                                gen_server:call(Worker, {push, DeviceToken, Notification}, Timeout)
                        end
                       );
push(PoolName, DeviceToken, Notification, Headers) ->
    poolboy:transaction(PoolName,
                        fun(Worker) ->
                                gen_server:call(Worker, {push, DeviceToken, Notification, Headers})
                        end
                       ).

push(PoolName, DeviceToken, Notification, Headers, Timeout) ->
    poolboy:transaction(PoolName,
                        fun(Worker) ->
                                gen_server:call(Worker, {push, DeviceToken, Notification, Headers}, Timeout)
                        end
                       ).
