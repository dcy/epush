## epush
epush是一个推送服务    
> 集成了苹果apns，小米xiaomi，华为hms，魅族Flyme，谷歌FCM的服务端推送    
> 同时也集成了云片yunpian的短信服务    

* 魅族Flyme: https://github.com/dcy/flyme_push
* Google Fcm: https://github.com/dcy/fcm_push
* 小米：https://github.com/dcy/xiaomi_push    
* ~~华为: https://github.com/dcy/huawei_push~~
* 华为HMS: https://github.com/dcy/hms_push
* 云片yunpian：https://github.com/dcy/eyunpian    
* 苹果apns: https://github.com/inaka/apns4erl    

## Why
* 在ios，程序进入后台会被系统中止，App进程无法处理消息
* 在Android，国内的很多定制的手机，App进程因为各种原因都无法保活，
* 关于第三方推送平台依然很多保活问题，阿里推送，友盟，环信，云巴等也有集成小米，华为等官方推送
* 小米，华为，魅族等的官方推送是系统级的，所以不用app进程常驻也能收到消息
* 相关讨论：[Android推送讨论](https://github.com/android-cn/topics/issues/4)

## How
* 当app中的长链接(MQTT,XMPP,Websocket等)因为不可抗拒的原因断开了或者没有长链接时调用
* 集成了小米，华为，魅族，Google FCM，苹果APNS，云片短信等server sdk
* 针对手机客户端调用对应厂商自己的系统级别官方推送，因为是系统级的，app进程不在也能收到消息
* epush提供两种工作方式，消息队列和HTTP请求方式
* 暂时消息队列用的是[Rabbitmq](https://www.rabbitmq.com/), 后续会增加[kafka](http://kafka.apache.org/)


## 使用方法
配置config/sys.config, 配置各个推送的参数：
```
{turtle, [
    {connection_config, [#{conn_name => amqp_server,
                              username => "username",
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
    {http_port, 8002},
    {push_confs, [
        #{id => apns1, type => apns, certfile => "priv/apns1_cert.pem", keyfile => "priv/apns1_key.pem", is_dev_env => false, headers => #{}, pool_size => 2, timeout => 5000},
        #{id => apns2, type => apns, certfile => "priv/apns2_cert.pem", keyfile => "priv/apns2_key.pem", is_dev_env => false, headers => #{apns_topic => "apns_topic"}, pool_size => 2, timeout => 5000},
        #{id => xiaomi1, type => xiaomi, pkg_name => "xiaomi1_pkg_name", app_secret => "xiaomi1_app_secret",
            pool_size => 6},
        #{id => hms1, type => hms, app_id => 123456, app_secret => "hms1_app_secret",
            pool_size => 6},
        #{id => fcm1, type => fcm, api_key=> "fcm1_api_key", proxy => {socks5, "127.0.0.1", 1080},
            pool_size => 6},
        #{id => flyme1, type => flyme, app_id => 123456, app_secret => "flyme1_app_secret",
            pool_size => 6},

        #{id => yunpian, type => yunpian, apikey => "yunpian_apikey", pool_size => 6}
    ]}
]},
```
### 推送
| 参数 | 描述          |
| ------------- | ----------- |
| epush_id      | sys.config的push_confs的id,标识用哪个推送来推（http方式用） |
| routing_key   | sys.config的push_confs的id,标识放入哪个队列（rabbitmq方式用）
| device_token  | 设备的token |
| push_method     | 通知栏general_notification, 透传general_app_msg |
| title | 标题（general_notification用）  |
| content | 内容 （general_notification用） |
| msg | 推送内容（general_app_msg用）|

#### HTTP方式
```bash
#通用通知栏
http post "http://localhost:8002/push" epush_id=xiaomi1 push_method=general_notification device_token="kuM7AixVXNQRUqgdEa5Zg+SWR0dNNeKbcg1ANYu/PFM=" title="title" content="content"

#通用投传
http post "http://localhost:8002/push" epush_id=apns1 push_method=general_app_msg device_token="9c3bba269163640bb0165ba9a8320959c7d4c70b0b8cb92ef3a9a9d1442fcccd" msg="app_msg"
```
> 其他接口参考：[/examples/http](/examples/http)

#### Rabbitmq方式
```Python
    def general_notification(self):
        data = {'push_method': 'general_notification',
                'device_token': 'kuM7AixVXNQRUqgdEa5Zg+SWR0dNNeKbcg1ANYu/PFM=',
                'title': 'title',
                'content': 'content'}
        self.in_mq(data)

    def general_app_msg(self):
        data = {'push_method': 'general_app_msg',
                'device_token': 'kuM7AixVXNQRUqgdEa5Zg+SWR0dNNeKbcg1ANYu/PFM=',
                'msg': 'app_msg'}
        self.in_mq(data)

    def in_mq(self, data):
        self.channel.basic_publish(exchange='',
                routing_key='xiaomi1',
                body=json.dumps(data))
```
> 其他接口参考：[/examples/rabbitmq](/examples/rabbitmq)


### 云片yunpian
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



## 运行
1. 安装rabbitmq, ```apt install rabbitmq-server```
2. 安装Erlang，```apt install erlang```
3. 获取epush: ```dcy@dcy-dcy:~/app$ git clone https://github.com/dcy/epush```
4. 更改配置，config/sys.config
5. 编译：```dcy@dcy-dcy:~/app/epush$ ./rebar3 release```
6. 进入bin目录：```dcy@dcy-dcy:~/app/epush$ cd _build/default/rel/epush```
7. 执行：```dcy@dcy-dcy:~/app/epush/_build/default/rel/epush$ ./bin/epush start```

## Todo:
- [ ] 增加kafka的工作方式
- [ ] Web管理后台


## 心得备注
* (2017-06-12 update):
* 小米的透传得在app打开才能收到，通知栏正常(透传是比较好的，唤醒App，app再处理逻辑)
* 华为推送请用hms，旧的推送效果很差，hms旧的rom要安装手机移动服务
