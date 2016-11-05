#!/bin/bash

# httpie: https://httpie.org/

TOKEN="cGP8QEX4ZLU:APA91bGP-Z5tqCVDCJf_KW7jtY2gq9DxmCCObN2JylndcX7MhMwRkSYJr4Ev1zrliIUZP2sJUsTl98m6aAHmcua6J15QjI59daAQyQ0ir1J35ywpH_Be5S5E4XEGwHh8z_3H2B89KQWV"

#通用透传接口
#http http://localhost:8002/push epush_id=fcm_c token=$TOKEN content="common" -f

#通知栏
#http http://localhost:8002/push  epush_id=fcm_c push_method=notification title=title content=content  to=$TOKEN

#透传
http http://localhost:8002/push  epush_id=fcm_c push_method=data content=content  to=$TOKEN

