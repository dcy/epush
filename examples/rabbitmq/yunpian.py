#!/usr/bin/env python
#coding:utf-8

import pika
import json

HOST = 'localhost'
USERNAME = 'hisir'
PASSWORD = 'hisir123'

class Yunpian():
    def __init__(self):
        credentials = pika.PlainCredentials(USERNAME, PASSWORD)
        self.connection = pika.BlockingConnection(pika.ConnectionParameters(host=HOST, credentials=credentials))
        self.channel = self.connection.channel()
        self.queue = 'yunpian'

    def single_send(self):
        data = {'type': 'single',
                'mobile': '+8615102025006',
		'content':'【广州灵光科技】验证码8888，请您尽快验证，完成Hisir注册。如非本人操作请忽略。'}
        self.in_mq(data)

    def batch_send(self):
        data = {'type': 'batch',
                'mobile': '15102025006',
		'content':'【广州灵光科技】验证码6666，请您尽快验证，完成Hisir注册。如非本人操作请忽略。'}
        self.in_mq(data)



    def end(self):
        self.channel.close()
        self.connection.close()

    def in_mq(self, data):
        self.channel.basic_publish(exchange='',
                routing_key=self.queue,
                body=json.dumps(data))


if __name__ == "__main__":
    yunpian = Yunpian()

    yunpian.single_send()
    #yunpian.batch_send()

    yunpian.end()
