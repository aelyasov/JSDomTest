#!/usr/bin/python

import pandas as pd
from glob import glob


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


def generateExcelReport(cs_name,
                        random_csvs,
                        genetic_csvs,
                        genetic_converge5_csvs,
                        genetic_converge10_csvs,
                        genetic_converge20_csvs):
    random_df = get_summary_report(random_csvs)
    genetic_df = get_summary_report(genetic_csvs)
    genetic_converge5_df = get_summary_report(genetic_converge5_csvs)
    genetic_converge10_df = get_summary_report(genetic_converge10_csvs)
    genetic_converge20_df = get_summary_report(genetic_converge20_csvs)
    concat_result = pd.concat([genetic_df,
                               genetic_converge5_df,
                               genetic_converge10_df,
                               genetic_converge20_df,
                               random_df],
                              axis=1)
    headers = ['gbrunch',
               'gmean',
               'gmax',
               'gmin',
               'gmedian',

               'g5brunch',
               'g5mean',
               'g5max',
               'g5min',
               'g5median',

               'g10brunch',
               'g10mean',
               'g10max',
               'g10min',
               'g10median',

               'g20brunch',
               'g20mean',
               'g20max',
               'g20min',
               'g20median',

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
    print 'csv_filename_split:', csv_filename_split
    key = '_'.join(csv_filename_split[0:(len(csv_filename_split) - 2)])
    print 'key:', key
    index.setdefault(key, list()).append(csv_fname)

print index

for cs_name, cs_filenames in index.iteritems():
    print 'casestudy:', cs_name
    print 'casestudy files:', cs_filenames
    tmp_index = {}
    for filename in cs_filenames:
        filename_split = filename.split('_')
        key = filename_split[len(filename_split) - 2]
        print key
        tmp_index.setdefault(key, list()).append(filename)
    generateExcelReport(cs_name,
                        tmp_index['random'],
                        tmp_index['genetic'],
                        tmp_index['geneticconverge5'],
                        tmp_index['geneticconverge10'],
                        tmp_index['geneticconverge20'])
writer.save()
