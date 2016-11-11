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
* 关于第三方推送平台依然很多保活问题，阿里推送，友盟，环信，云巴等也有集成小米，华为等官方推送
* 小米，华为，魅族等的官方推送是系统级的，所以不用app进程常驻也能收到消息
* 相关讨论：[Android推送讨论](https://github.com/android-cn/topics/issues/4)

##How
* 当app中的长链接(MQTT,XMPP,Websocket等)因为不可抗拒的原因断开了或者没有长链接时调用
* 集成了小米，华为，魅族，Google FCM，苹果APNS，云片短信等server sdk
* 针对手机客户端调用对应厂商自己的系统级别官方推送，因为是系统级的，app进程不在也能收到消息
* epush提供两种工作方式，消息队列和HTTP请求方式
* 暂时消息队列用的是[Rabbitmq](https://www.rabbitmq.com/), 后续会增加[kafka](http://kafka.apache.org/)


##使用方法
配置config/sys.config：
```
{turtle, [
    {connection_config, [#{conn_name => amqp_server,
                              username => "hisir",
                              password => "hisir123",
                              virtual_host => "/",
                              connections => [
                              {main, [
                                  {"localhost", 5672 } ]},
                              {backup, [
                                  {"localhost", 5672 } ]} ]
                          }]}
]},

{epush, [
    {http_port, 8002},
    {push_confs, [
        #{id => apns_c, type => apns, cert_file => "priv/cert1.pem", is_sandbox_env => false,
            pool_size => 6},
        #{id => apns_t, type => apns, cert_file => "priv/cert2.pem", is_sandbox_env => false,
            pool_size => 6},
        #{id => xiaomi_c, type => xiaomi, pkg_name => "xiaomi_pkg_name", app_secret => "xiaomi_app_secret",
            pool_size => 6},
        #{id => huawei_c, type => huawei, app_id => 123456, app_secret => "huawei_app_secret",
            pool_size => 6},
        %#{id => fcm_t, type => fcm, api_key=> "fcm_api_key", proxy => undefined,
        %   pool_size => 6}
        #{id => fcm_c, type => fcm, api_key=> "fcm_api_key", proxy => "127.0.0.1:1081",
            pool_size => 6},
        #{id => flyme_c, type => flyme, app_id => 123456, app_secret => "flyme_app_secret",
            pool_size => 6},
        #{id => yunpian, type => sms, sms_type => yunpian, apikey => "yunpian_apikey",
            pool_size => 6}
    ]}
]},
```

###苹果APNS
* http: [/examples/http/apns.sh](/examples/http/apns.sh)
```bash
#通用透传接口
http http://localhost:8002/push epush_id=apns_c token=$TOKEN content="common" -f

#各种参数的接口
http http://localhost:8002/push epush_id=apns_c device_token=$TOKEN  sound="default"
```

* Rabbitmq: [/examples/rabbitmq/apns.py](/examples/rabbitmq/apns.py)
```Python
#通用
def common(self):
    msg_content = json.dumps({'body': 'body'})
    message = json.dumps({'message_type': 'Common', 'type': 0, 'from': 51, 'content': msg_content})
    data = {'content': message,
            'token': self.token
            }
    self.in_mq(data)
```

###谷歌FCM
* Http: [/examples/http/fcm.sh](/examples/http/fcm.sh)
```bash
#通用透传接口
#http http://localhost:8002/push epush_id=fcm_c token=$TOKEN content="common" -f

#通知栏
#http http://localhost:8002/push  epush_id=fcm_c push_method=notification title=title content=content  to=$TOKEN

#透传
http http://localhost:8002/push  epush_id=fcm_c push_method=data content=content  to=$TOKEN
```
* Rabbitmq: [/examples/rabbitmq/fcm.py](/examples/rabbitmq/fcm.py)
```Python
#通用
def common(self):
    msg_content = json.dumps({'body': 'body'})
    message = json.dumps({'message_type': 'Common', 'type': 0, 'from': 51, 'content': msg_content})
    data = {'content': message,
            'token': self.token
            }
    self.in_mq(data)

#通知栏
def notification(self):
    data = {'push_method': 'notification',
            'title': 'Title',
            'content': 'Content',
            'to': self.token 
            }
    self.in_mq(data)

#透传
def data(self):
    msg_content = json.dumps({'body': 'body'})
    message = json.dumps({'message_type': 'Common', 'type': 0, 'from': 51, 'content': msg_content})
    msg = {'push_method': 'data',
            'content': message,
            'to': self.token
            }
    self.in_mq(msg)

#主题
def topics(self):
    data = {'push_method': 'topics',
    'topics': ["/topics/foo-bar"],
            'content': "content"
            }
    self.in_mq(data)
```
###小米
* http: [/examples/http/xiaomi.sh](/examples/http/xiaomi.sh)
```bash
#通用透传接口
#http http://localhost:8002/push epush_id=xiaomi_c token="go6VssZlTDDypm+hxYdaxycXtqM7M9NsTPbCjzyIyh0=" content="common" -f

#推送全部
#http http://localhost:8002/push epush_id=xiaomi_c push_method=all title="all中文" description="中文"

#推送单人通知 
http http://localhost:8002/push epush_id=xiaomi_c push_method=notification_send title="notification_send中文" description="中文" registration_id="go6VssZlTDDypm+hxYdaxycXtqM7M9NsTPbCjzyIyh0=" -f

#推送单人透传 
#http http://localhost:8002/push epush_id=xiaomi_c push_method=pass_through description="中文" registration_id="go6VssZlTDDypm+hxYdaxycXtqM7M9NsTPbCjzyIyh0=" -f
```

* Rabbitmq: [/examples/rabbitmq/xiaomi.py](/examples/rabbitmq/xiaomi.py)
```Python
def notification_send(self):
    data = {'push_method': 'notification_send',
            'title': 'Test  中文',
            'description': 'Content',
            'registration_id': 'go6VssZlTDDypm+hxYdaxycXtqM7M9NsTPbCjzyIyh0='}
    self.in_mq(data)

def all(self):
    data = {'push_method':'all',
            'title':'Test中文',
            'description':'Test'}
    self.in_mq(data)
```

###华为
* http: [/examples/http/huawei.sh](/examples/http/huawei.sh)
```bash
#通用透传接口
#http http://localhost:8002/push epush_id=huawei_c token=$TOKEN content="common" -f

#通知栏
http http://localhost:8002/push epush_id=huawei_c push_method=notification_send title=title content=content  tokens=$TOKEN

#透传
#http http://localhost:8002/push  epush_id=huawei_c push_method=single_send message=message  deviceToken=$TOKEN
```
* Rabbitmq: [/examples/rabbitmq/huawei.py](/examples/rabbitmq/huawei.py)
```Python
#通知栏
def notification(self):
    data = {'push_method':'notification_send',
            'push_type': 1,
            'tokens': '08670650250202362000003019000001',
            'title': "Hello!",
            'content': "World"
            }
    self.in_mq(data)

#透传
def single(self):
    msg_content = json.dumps({'body': 'body'})
    message = json.dumps({'message_type': 'Common', 'type': 0, 'from': 51, 'content': msg_content})
    data = {'push_method': 'single_send',
            'message': message,
            'deviceToken': '08670650250202362000003019000001'
            }
    self.in_mq(data)

#群发
def batch(self):
    msg_content = json.dumps({'body': 'body'})
    message = json.dumps({'message_type': 'Common', 'type': 0, 'from': 51, 'content': msg_content})
    #message = json.dumps({'test': 'test'})
    data = {'push_method': 'batch_send',
            'message': message,
            'deviceTokenList': ['08670650250202362000003019000001', 'sdfsdf']
            }
    self.in_mq(data)
```
###魅族Flyme
* http: [/examples/http/flyme.sh](/examples/http/flyme.sh)
```bash
#通用透传接口
#http http://localhost:8002/push epush_id=flyme_c token=$TOKEN content="common" -f

#通知栏
#http http://localhost:8002/push  epush_id=flyme_c push_method=varnished title=title content=content  pushIds=$TOKEN
#http http://localhost:8002/push  epush_id=flyme_c push_method=notification title=title content=content  pushIds=$TOKEN

#透传
http http://localhost:8002/push  epush_id=flyme_c push_method=unvarnished title=title content=content  pushIds=$TOKEN
```

* Rabbitmq: [/examples/rabbitmq/flyme.py](/examples/rabbitmq/flyme.py)
```Python
#通知栏
def notification(self):
    data = {'push_method':'notification',
            'title': "Hello!",
            'content': "World",
            'pushIds': self.push_ids
            }
    self.in_mq(data)

#透传
def unvarnished(self):
    msg_content = json.dumps({'body': 'body'})
    message = json.dumps({'message_type': 'Common', 'type': 0, 'from': 51, 'content': msg_content})
    data = {'push_method': 'unvarnished',
            'content': message,
            'pushIds': 'UU34b4f75595d58540a78407f4d5a60630642497c5c5e'
            }
    self.in_mq(data)
```
###云片yunpian
* http: [/examples/http/yunpian.sh](/examples/http/yunpian.sh)
```bash
#单发
#http http://localhost:8002/push epush_id=yunpian type=single mobile=$MOBILE content=$CONTENT

#群发
http http://localhost:8002/push epush_id=yunpian type=batch mobile=$MOBILE content=$CONTENT
```

* Rabbitmq: [/examples/rabbitmq/yunpian.py](/examples/rabbitmq/yunpian.py)
```Python
def single_send(self):
    data = {'type': 'single',
            'mobile': '+861510202',
    'content':'【广州科技】验证码8888，请您尽快验证，完成sir注册。如非本人操作请忽略。'}
    self.in_mq(data)

def batch_send(self):
    data = {'type': 'batch',
            'mobile': '1510202',
    'content':'【广州科技】验证码6666，请您尽快验证，完成sir注册。如非本人操作请忽略。'}
```



##运行
1. 安装rabbitmq, ```apt install rabbitmq-server```
2. 安装Erlang，```apt install erlang```
3. 获取epush: ```dcy@dcy-dcy:~/app$ git clone https://github.com/dcy/epush```
4. 更改配置，config/sys.config
5. 编译：```dcy@dcy-dcy:~/app/epush$ ./rebar3 release```
6. 进入bin目录：```dcy@dcy-dcy:~/app/epush$ cd _build/default/rel/epush```
7. 执行：```dcy@dcy-dcy:~/app/epush/_build/default/rel/epush$ ./bin/epush start```

##Todo:
- [x] 增加HTTP请求方式
- [ ] 增加华为新的推送HMS Push
- [ ] 增加联想的官方推送
- [ ] 增加kafka的工作方式
- [ ] Web管理后台


##心得备注
* (2016-10-21 update):
* 小米的透传得在app打开才能收到，通知栏正常(透传是比较好的，唤醒App，app再处理逻辑)
* 华为的推送重启手机后，得点一次APP，或者设置开机自启动后才正常收到，之后杀掉进程也可以
* FCM的文档没看到服务端subscribe的api，android的基本都有
* 华为推出了HMS
