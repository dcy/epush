#!/bin/bash

# httpie: https://httpie.org/

#通用透传接口
#http http://localhost:8002/push epush_id=xiaomi_c token="go6VssZlTDDypm+hxYdaxycXtqM7M9NsTPbCjzyIyh0=" content="common" -f

#推送全部
#http http://localhost:8002/push epush_id=xiaomi_c push_method=all title="all中文" description="中文"

#推送单人通知 
http http://localhost:8002/push epush_id=xiaomi_c push_method=notification_send title="notification_send中文" description="中文" registration_id="go6VssZlTDDypm+hxYdaxycXtqM7M9NsTPbCjzyIyh0=" -f

#推送单人透传 
#http http://localhost:8002/push epush_id=xiaomi_c push_method=pass_through description="中文" registration_id="go6VssZlTDDypm+hxYdaxycXtqM7M9NsTPbCjzyIyh0=" -f
