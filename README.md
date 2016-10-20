##epush
epush是一个推送服务    
> 集成了苹果apns，小米xiaomi，华为huawei，魅族Flyme，谷歌FCM的服务端推送    
> 同时也集成了云片yunpian的短信服务    

* 魅族Flyme: https://github.com/dcy/flyme_push
* Google Fcm: https://github.com/dcy/fcm_push
* 小米：https://github.com/dcy/xiaomi_push    
* 华为: https://github.com/dcy/huawei_push
* 云片yunpian：https://github.com/dcy/eyunpian    
* 苹果apns: https://github.com/inaka/apns4erl    

##Why
* 在ios，程序进入后台会被系统中止，App进程无法处理消息
* 在Android，国内的很多定制的手机，App进程因为各种原因都无法保活，
* 关于第三方推送平台，看了很多案例，依然很多保活问题，阿里推送，友盟，环信，云巴等也有集成小米，华为等官方推送
* 小米，华为，魅族等的官方推送是系统级的，所以不用app进程常驻也能收到消息
* 相关讨论：[Android推送讨论](https://github.com/android-cn/topics/issues/4)

##How
* 集成了小米，华为，魅族，Google FCM，苹果APNS，云片短信等server sdk
* 针对手机客户端调用对应厂商自己的系统级别官方推送，因为是系统级的，哪怕app进程不在也能收到消息
* epush提供两种工作方式，消息队列和HTTP请求方式
* 暂时消息队列用的是[Rabbitmq](https://www.rabbitmq.com/), 后续会增加[kafka](http://kafka.apache.org/)


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
        #{type => apns, cert_file => "priv/cert1.pem", is_sandbox_env => false,
            pool_size => 6, queue => <<"apns1">>},
        #{type => apns, cert_file => "priv/cert2.pem", is_sandbox_env => false,
            pool_size => 6, queue => <<"apns2">>},
        #{type => xiaomi, pkg_name => "xiaomi_pkg_name", app_secret => "xiaomi_app_secret",
            pool_size => 6, queue => <<"xiaomi1">>},
        #{type => huawei, app_id => 123456, app_secret => "huawei_app_secret",
            pool_size => 6, queue => <<"huawei1">>},
        %#{type => fcm, app_secret => "fcm_app_secret", proxy => undefined,
        %   pool_size => 6, queue => <<"fcm1">>}
        #{type => fcm, app_secret => "fcm_app_secret", proxy => "127.0.0.1:1081",
            pool_size => 6, queue => <<"fcm2">>},
        #{type => flyme, app_id => 12345, app_secret => "flyme_app_secret",
            pool_size => 6, queue => <<"flyme1">>},
        #{type => flyme, app_id => 12345, app_secret => "flyme_app_secret",
            pool_size => 6, queue => <<"flyme2">>},
        #{type => sms, sms_type => yunpian, apikey => "yunpian_apikey",
            pool_size => 6, queue => <<"yunpian">>}
    ]}
]},
```

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

##Todo:
- [ ] 整理完善文档
- [ ] 增加HTTP请求方式
- [ ] 增加kafka的工作方式
- [ ] 增加联想的官方推送
- [ ] Web管理后台
