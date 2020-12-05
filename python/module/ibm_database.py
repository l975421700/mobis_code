#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Jul 19 10:13:41 2020

@author: gao
"""


from cloudant.client import Cloudant
from cloudant.error import CloudantException
from cloudant.result import Result, ResultByKey

client = Cloudant.iam("dba86af3-caa9-4e6f-8010-1c981511666f-bluemix", "ed58f9qsp-V2j_1ETew5tIDvTilHyEWpSEpWWcZ3C28U")
client.connect()

database_name = "trip_info_imputation"

my_database = client.create_database(database_name)

if my_database.exists():
   print(f"'{database_name}' successfully created.")
   
sample_data = [
   [1, "one", "boiling", 100],
   [2, "two", "hot", 40],
   [3, "three", "warm", 20],
   [4, "four", "cold", 10],
   [5, "five", "freezing", 0]
 ]


client.disconnect()




