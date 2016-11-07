#!/bin/bash

# httpie: https://httpie.org/

TOKEN="UU34b4f75595d58540a78407f4d5a60630642497c5c5e"

#通用透传接口
#http http://localhost:8002/push epush_id=flyme_c token=$TOKEN content="common" -f

#通知栏
#http http://localhost:8002/push  epush_id=flyme_c push_method=varnished title=title content=content  pushIds=$TOKEN
#http http://localhost:8002/push  epush_id=flyme_c push_method=notification title=title content=content  pushIds=$TOKEN

#透传
http http://localhost:8002/push  epush_id=flyme_c push_method=unvarnished title=title content=content  pushIds=$TOKEN
