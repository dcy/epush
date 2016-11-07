#!/usr/bin/env python
#coding:utf-8

import pika
import json
import urllib

HOST = 'localhost'
USERNAME = 'hisir'
PASSWORD = 'hisir123'

class Fcm():
    def __init__(self):
        credentials = pika.PlainCredentials(USERNAME, PASSWORD)
        self.connection = pika.BlockingConnection(pika.ConnectionParameters(host=HOST, credentials=credentials))
        self.channel = self.connection.channel()
        self.queue = 'fcm_c'
        self.token = 'cGP8QEX4ZLU:APA91bGP-Z5tqCVDCJf_KW7jtY2gq9DxmCCObN2JylndcX7MhMwRkSYJr4Ev1zrliIUZP2sJUsTl98m6aAHmcua6J15QjI59daAQyQ0ir1J35ywpH_Be5S5E4XEGwHh8z_3H2B89KQWV'

    #通用
    def common(self):
        msg_content = json.dumps({'body': 'body'})
        message = json.dumps({'message_type': 'Common', 'type': 0, 'from': 51, 'content': msg_content})
        data = {'content': message,
                'token': self.token
                }
        self.in_mq(data)


    #通知栏
    def notification(self):
        data = {'push_method': 'notification',
                'title': 'Title',
                'content': 'Content',
                'to': self.token 
                }
        self.in_mq(data)

    #透传
    def data(self):
        msg_content = json.dumps({'body': 'body'})
        message = json.dumps({'message_type': 'Common', 'type': 0, 'from': 51, 'content': msg_content})
        msg = {'push_method': 'data',
                'content': message,
                'to': self.token
                }
        self.in_mq(msg)

    #主题
    def topics(self):
        data = {'push_method': 'topics',
		'topics': ["/topics/foo-bar"],
                'content': "content"
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
    fcm = Fcm()

    #fcm.common()
    fcm.notification()
    #fcm.data()
    #fcm.topics()

    fcm.end()
