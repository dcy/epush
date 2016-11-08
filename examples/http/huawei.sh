#!/bin/bash

# httpie: https://httpie.org/

TOKEN="08670650250202362000003019000001"

#通用透传接口
#http http://localhost:8002/push epush_id=huawei_c token=$TOKEN content="common" -f

#通知栏
http http://localhost:8002/push epush_id=huawei_c push_method=notification_send title=title content=content  tokens=$TOKEN

#透传
#http http://localhost:8002/push  epush_id=huawei_c push_method=single_send message=message  deviceToken=$TOKEN
