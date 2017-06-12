-module(epush_util).
-export([get_push_conf/1, put_push_conf/2]).


get_push_conf(EpushId) ->
    eutil:get_ets(epush_confs, {push_confs, EpushId}).

put_push_conf(Id, Conf) ->
    eutil:put_ets(epush_confs, {push_confs, eutil:to_binary(Id)}, Conf).
