#!/usr/bin/python

import pandas as pd

a = pd.read_csv('RequiredField.csv', sep=';')
print a
b = pd.read_csv('RequiredField1.csv', sep=';')
print b
b = b.drop('brunch', axis=1)
result = a['brunch'].to_frame()
print result
merged = pd.concat([a,b], axis=1)
meged_numeric = merged.apply(pd.to_numeric, errors='coerce')
result['mean'] = meged_numeric.mean(axis=1)
result['max'] = meged_numeric.max(axis=1)
result['min'] = meged_numeric.min(axis=1)
result['median'] = meged_numeric.median(axis=1)
print result
headers = ['brunch', 'mean', 'max', 'min', 'median']
result.to_csv('output.csv', index=False, header=headers)

writer = pd.ExcelWriter('result.xlsx', engine='xlsxwriter')
result.to_excel(writer, sheet_name='RequiredField')
writer.save()
