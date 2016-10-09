#!/usr/bin/env python
#coding:utf-8

import pika
import json
import urllib

HOST = 'localhost'
USERNAME = 'hisir'
PASSWORD = 'hisir123'


class Huawei():
    def __init__(self):
        credentials = pika.PlainCredentials(USERNAME, PASSWORD)
        self.connection = pika.BlockingConnection(pika.ConnectionParameters(host=HOST, credentials=credentials))
        self.channel = self.connection.channel()

    #通知栏
    def notification(self):
        data = {'push_method':'notification_send',
                'push_type': 1,
                'tokens': '08670650250202362000003019000001',
                'title': "Hello!",
                'content': "World"
                }
        self.in_mq(data)

    #透传
    def single(self):
        msg_content = json.dumps({'body': 'body'})
        message = json.dumps({'message_type': 'Common', 'type': 0, 'from': 51, 'content': msg_content})
        data = {'push_method': 'single_send',
                'message': message,
                'deviceToken': '08670650250202362000003019000001'
                }
        self.in_mq(data)

    #群发
    def batch(self):
        msg_content = json.dumps({'body': 'body'})
        message = json.dumps({'message_type': 'Common', 'type': 0, 'from': 51, 'content': msg_content})
        #message = json.dumps({'test': 'test'})
        data = {'push_method': 'batch_send',
                'message': message,
                'deviceTokenList': ['08670650250202362000003019000001', 'sdfsdf']
                }
        self.in_mq(data)



    def end(self):
        self.channel.close()
        self.connection.close()


    def in_mq(self, data):
        self.channel.basic_publish(exchange='',
                routing_key='huawei_c',
                body=json.dumps(data))



if __name__ == "__main__":
    huawei = Huawei()

    #huawei.notification()
    #huawei.single()
    huawei.batch()

    huawei.end()
