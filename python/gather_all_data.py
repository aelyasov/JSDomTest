#!/usr/bin/python

import pandas as pd
from glob import glob
from os.path import splitext

writer = pd.ExcelWriter('all_data.xlsx', engine='xlsxwriter')

files = glob('*.csv')
print "Total number of csv files:", len(files)
for file in files:
    print "Processing file", file
    df = pd.read_csv(file, sep=';')
    fname = splitext(file)[0]
    df.to_excel(writer, index=False, sheet_name=fname)
writer.save()




