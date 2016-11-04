#!/bin/bash

# httpie: https://httpie.org/

#推送全部
#http http://localhost:8002/push epush_id=xiaomi_c push_method=all title="hello中文" description="中文"

#推送单人 
http http://localhost:8002/push epush_id=xiaomi_c push_method=notification_send title="notification_send中文" description="中文" registration_id="go6VssZlTDDypm+hxYdaxycXtqM7M9NsTPbCjzyIyh0=" -f
