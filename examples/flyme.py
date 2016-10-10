#!/usr/bin/env python
#coding:utf-8

import pika
import json
import urllib

HOST = 'localhost'
USERNAME = 'hisir'
PASSWORD = 'hisir123'


class Flyme():
    def __init__(self):
        credentials = pika.PlainCredentials(USERNAME, PASSWORD)
        self.connection = pika.BlockingConnection(pika.ConnectionParameters(host=HOST, credentials=credentials))
        self.channel = self.connection.channel()

    #通知栏
    def varnished(self):
        data = {'push_method':'varnished',
                'title': "Hello!",
                'content': "World",
                'pushIds': 'UU34b4f75595d58540a78407f4d5a60630642497c5c5e'
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


    def end(self):
        self.channel.close()
        self.connection.close()


    def in_mq(self, data):
        self.channel.basic_publish(exchange='',
                routing_key='flyme_c',
                body=json.dumps(data))



if __name__ == "__main__":
    flyme = Flyme()

    #flyme.unvarnished()
    flyme.varnished()


    flyme.end()
