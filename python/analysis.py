#!/usr/bin/python

import pandas as pd
from glob import glob

# a = pd.read_csv('RequiredField.csv', sep=';')
# print a
# b = pd.read_csv('RequiredField1.csv', sep=';')
# print b
# b = b.drop('brunch', axis=1)
# result = a['brunch'].to_frame()
# print result
# merged = pd.concat([a, b], axis=1)
# meged_numeric = merged.apply(pd.to_numeric, errors='coerce')
# result['mean'] = meged_numeric.mean(axis=1)
# result['max'] = meged_numeric.max(axis=1)
# result['min'] = meged_numeric.min(axis=1)
# result['median'] = meged_numeric.median(axis=1)
# print result
# headers = ['brunch', 'mean', 'max', 'min', 'median']
# result.to_csv('output.csv', index=False, header=headers)

# writer = pd.ExcelWriter('result.xlsx', engine='xlsxwriter')
# result.to_excel(writer, sheet_name='RequiredField')
# writer.save()


# random_csvs = ['validateNumber_random_1.csv',
#                'validateNumber_random_2.csv',
#                'validateNumber_random_3.csv']

# genetic_csvs = ['validateNumber_genetic_1.csv',
#                 'validateNumber_genetic_2.csv',
#                 'validateNumber_genetic_3.csv']


def get_summary_report(csvs):
    result = pd.read_csv(csvs[0], sep=';')['brunch'].to_frame()
    dataframes = []

    for csv in csvs:
        dataframe = pd.read_csv(csv, sep=';').drop('brunch', axis=1)
        dataframes.append(dataframe)

    merged_dataframes = pd.concat(dataframes, axis=1) \
                          .apply(pd.to_numeric, errors='coerce')
    result['mean'] = merged_dataframes.mean(axis=1)
    result['max'] = merged_dataframes.max(axis=1)
    result['min'] = merged_dataframes.min(axis=1)
    result['median'] = merged_dataframes.median(axis=1)
    return result


def generateExcelReport(cs_name, random_csvs, genetic_csvs):
    genetic_df = get_summary_report(genetic_csvs)
    random_df = get_summary_report(random_csvs)
    concat_result = pd.concat([genetic_df, random_df], axis=1)
    headers = ['gbrunch',
               'gmean',
               'gmax',
               'gmin',
               'gmedian',

               'rbrunch',
               'rmean',
               'rmax',
               'rmin',
               'rmedian']
    # concat_result.ix[concat_result['brunch'] != 'starttime']
    concat_result.to_excel(writer,
                           index=False,
                           sheet_name=cs_name,
                           header=headers)


writer = pd.ExcelWriter('all_data.xlsx', engine='xlsxwriter')
all_csvs = glob('*.csv')
index = {}
for csv_fname in all_csvs:
    csv_filename_split = csv_fname.split('_')
    key = '_'.join(csv_filename_split[0:(len(csv_filename_split) - 2)])
    index.setdefault(key, list()).append(csv_fname)

# print index

for cs_name, cs_filenames in index.iteritems():
    print 'casestudy:', cs_name
    print 'casestudy files:', cs_filenames
    tmp_index = {}
    for filename in cs_filenames:
        filename_split = filename.split('_')
        key = filename_split[len(filename_split) - 2]
        print key
        tmp_index.setdefault(key, list()).append(filename)
    generateExcelReport(cs_name, tmp_index['random'], tmp_index['genetic'])
writer.save()
