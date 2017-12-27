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
    ## print merged_dataframes
    return (merged_dataframes, result)


def a12slow(lst1, lst2):
    more = same = 0.0
    for x in sorted(lst1):
        for y in sorted(lst2):
            if x == y:
                same += 1
            elif x > y:
                more += 1
    return (more + 0.5*same) / (len(lst1)*len(lst2))

def mk_a12_df(df1, df2):
    a12 = []
    for index, row in df1.iterrows():
        a12.append(a12slow(df1.iloc[index], df2.iloc[index]))
    a12_df = pd.DataFrame(a12, columns=['a12'])
    print(a12_df)
    return a12_df


def generateExcelReport(cs_name,
                        random_csvs,
                        genetic_csvs,
                        genetic_converge5_csvs,
                        genetic_converge10_csvs,
                        genetic_converge20_csvs):
    (mrandom_df, random_df) = get_summary_report(random_csvs)
    (mgenetic_df, genetic_df) = get_summary_report(genetic_csvs)
    (mgenetic_converge5_df, genetic_converge5_df) = get_summary_report(genetic_converge5_csvs)
    (mgenetic_converge10_df, genetic_converge10_df) = get_summary_report(genetic_converge10_csvs)
    (mgenetic_converge20_df, genetic_converge20_df) = get_summary_report(genetic_converge20_csvs)

    rg_df = mk_a12_df(mrandom_df, mgenetic_df)
    rg5_df = mk_a12_df(mrandom_df, mgenetic_converge5_df)
    rg10_df = mk_a12_df(mrandom_df, mgenetic_converge10_df)
    gg5_df = mk_a12_df(mgenetic_df, mgenetic_converge5_df)
    gg10_df = mk_a12_df(mgenetic_df, mgenetic_converge10_df)
    g5g10_df = mk_a12_df(mgenetic_converge5_df, mgenetic_converge10_df)

    concat_result = pd.concat([genetic_df,
                               genetic_converge5_df,
                               genetic_converge10_df,
                               genetic_converge20_df,
                               random_df,
                               rg_df, rg5_df, rg10_df, gg5_df, gg10_df, g5g10_df],
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
               'rmedian',

               'R-G', 'R-G5', 'R-G10', 'G-G5', 'G-G10', 'G5-G10']
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
    print('csv_filename_split:', csv_filename_split)
    key = '_'.join(csv_filename_split[0:(len(csv_filename_split) - 2)])
    print('key:', key)
    index.setdefault(key, list()).append(csv_fname)

print(index)

for cs_name, cs_filenames in index.items():
    print('casestudy:', cs_name)
    print('casestudy files:', cs_filenames)
    tmp_index = {}
    for filename in cs_filenames:
        filename_split = filename.split('_')
        key = filename_split[len(filename_split) - 2]
        print(key)
        tmp_index.setdefault(key, list()).append(filename)
    generateExcelReport(cs_name,
                        tmp_index['random'],
                        tmp_index['genetic'],
                        tmp_index['geneticconverge5'],
                        tmp_index['geneticconverge10'],
                        tmp_index['geneticconverge20'])
writer.save()
