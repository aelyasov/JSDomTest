#!/usr/bin/python

import pandas as pd

a = pd.read_csv('RequiredField.csv', sep=';')
# print a
b = pd.read_csv('RequiredField1.csv', sep=';')
# print b
b = b.drop('brunch', axis=1)
merged = pd.concat([a,b], axis=1)
print b.iloc[14:15]
print b.iloc[14:15].mean(axis=1)
# print merged.iloc[:,1:21]
merged['mean'] = merged.iloc[:,1:1].mean(axis=1)
merged.to_csv('output.csv')

