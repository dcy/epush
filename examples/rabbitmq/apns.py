#!/usr/bin/env python
#coding:utf-8

import pika
import json
import urllib

HOST = 'localhost'
USERNAME = 'hisir'
PASSWORD = 'hisir123'

class Apns():
    def __init__(self):
        credentials = pika.PlainCredentials(USERNAME, PASSWORD)
        self.connection = pika.BlockingConnection(pika.ConnectionParameters(host=HOST, credentials=credentials))
        self.channel = self.connection.channel()
        self.queue = 'apns_c'
        self.token = '6765c4a0c68fd2ea0c5a5f6ff9eb01dcaae68a647ee4e643c3cf169455dae206'

    #通用
    def common(self):
        msg_content = json.dumps({'body': 'body'})
        message = json.dumps({'message_type': 'Common', 'type': 0, 'from': 51, 'content': msg_content})
        data = {'content': message,
                'token': self.token
                }
        self.in_mq(data)


    def end(self):
        self.channel.close()
        self.connection.close()

    def in_mq(self, data):
        self.channel.basic_publish(exchange='',
                routing_key=self.queue,
                body=json.dumps(data))


if __name__ == "__main__":
    apns = Apns()

    apns.common()

    apns.end()
