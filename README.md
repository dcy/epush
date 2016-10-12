##epush
epush是一个推送服务，集成了苹果apns，小米xiaomi，华为huawei，魅族Flyme，谷歌FCM的服务端推送，同时也集成了云片yunpian的短信服务    
> 会逐步拆成下面的组件：

* 魅族Flyme: https://github.com/dcy/flyme_push
* Google Fcm: https://github.com/dcy/fcm_push
* 小米：https://github.com/dcy/xiaomi_push (todo)
* 华为: https://github.com/dcy/huawei_push (todo)
* 云片yunpian：https://github.com/dcy/eyunpian （todo)
* 苹果apns: https://github.com/inaka/apns4erl    



##使用方法
配置config/sys.config：
```
{turtle, [
    {connection_config, [#{conn_name => amqp_server,
                              username => "name",
                              password => "password",
                              virtual_host => "/",
                              connections => [
                              {main, [
                                  {"localhost", 5672 } ]},
                              {backup, [
                                  {"localhost", 5672 } ]} ]
                          }]}
]},

{epush, [
    {push_confs, [
        #{type => apns, pool_size => 6, queue => <<"apns_c">>, cert_file => "priv/cert1.pem", is_sandbox_env => false},
        #{type => apns, pool_size => 6, queue => <<"apns_t">>, cert_file => "priv/cert2.pem", is_sandbox_env => false},
        #{type => xiaomi, pkg_name => "xiaomi_pkg_name", app_secret => "xiaomi_app_secret",
            pool_size => 6, queue => <<"xiaomi_c">>},
        #{type => huawei, app_id => 123456, app_secret => "huawei_app_secret",
            pool_size => 6, queue => <<"huawei_c">>},
        %#{type => fcm, app_secret => "fcm_app_secret", proxy => undefined,
        %   pool_size => 6, queue => <<"fcm_t">>}
        #{type => fcm, app_secret => "fcm_app_secret", proxy => "127.0.0.1:1081",
            pool_size => 6, queue => <<"fcm_c">>},
        #{type => flyme, app_id => 12345, app_secret => "flyme_app_secret",
            pool_size => 6, queue => <<"flyme_c">>},
        #{type => flyme, app_id => 12345, app_secret => "flyme_app_secret",
            pool_size => 6, queue => <<"flyme_t">>},
        #{type => sms, sms_type => yunpian, apikey => "yunpian_apikey",
            pool_size => 6, queue => <<"yunpian">>}
    ]}
]},
```
epush提供两种方式，一种直接放入消息队列，epush去队列获取相应数据去推送；另一种是通过http调用;

###消息队列Rabbitmq
####苹果APNS
####谷歌FCM
####小米
####华为
####魅族Flyme
####云片yunpian

###Http请求方式


##运行
1. 安装rabbitmq, ```apt install rabbitmq-server```
2. 安装Erlang，```apt install erlang```
3. 获取epush: ```dcy@dcy-dcy:~/app$ git clone https://github.com/dcy/epush```
4. 更改配置，config/sys.config
5. 编译：```dcy@dcy-dcy:~/app/epush$ ./rebar3 release```
6. 进入bin目录：```dcy@dcy-dcy:~/app/epush$ cd _build/default/rel/epush```
7. 执行：```dcy@dcy-dcy:~/app/epush/_build/default/rel/epush$ ./bin/epush start```
