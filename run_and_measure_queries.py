#!/usr/bin/env python

import sys
from pathlib import Path
from time import time
import pyodbc
import json
import csv

# Open parameters file and read them
with open(str(Path.cwd())+'/TPCDS_parameters.json') as file:
    parameters = json.load(file)
    server = (parameters['SQLSERVER_name'])
    db = (parameters['DB_name'])
    output_file_name = (parameters['output_file_name'])

#Connect to the SQL Server DB for the TPC-DS datawarehouse
conn = pyodbc.connect('Driver={/opt/microsoft/msodbcsql17/lib64/libmsodbcsql-17.4.so.2.1};SERVER='+server+';DATABASE='+db+';UID=sqlserver;PWD=123') 
cursor = conn.cursor()

#Creates the .csv file with the query execution times.
with open(str(output_file_name) + '.csv', 'w', newline='') as f:
    fieldnames = ['query_number', 'exec_time']
    writer = csv.DictWriter(f, fieldnames=fieldnames) 
    writer.writeheader()
    query_count = 0   
    with open(str(Path.cwd())+'/tpcds-queries.sql', "r") as inp:
        for statement in inp.read().split(';--'):
            tic = time()
            cursor.execute(statement)
            toc = time()
            query_count += 1
            writer.writerow({'query_number':'Query ' + str(query_count), 'exec_time': toc - tic})