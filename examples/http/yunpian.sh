#!/bin/bash

# httpie: https://httpie.org/

MOBILE="1510202"
CONTENT="【广州科技】验证码8888，请您尽快验证，完成sir注册。如非本人操作请忽略。"

#单发
#http http://localhost:8002/push epush_id=yunpian type=single mobile=$MOBILE content=$CONTENT

#群发
http http://localhost:8002/push epush_id=yunpian type=batch mobile=$MOBILE content=$CONTENT
