#!/bin/bash

# httpie: https://httpie.org/

TOKEN="6765c4a0c68fd2ea0c5a5f6ff9eb01dcaae68a647ee4e643c3cf169455dae206"

#通用透传接口
http http://localhost:8002/push epush_id=apns_c token=$TOKEN content="common" -f


#各种参数的接口
http http://localhost:8002/push epush_id=apns_c device_token=$TOKEN  sound="default"
