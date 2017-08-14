#!/usr/bin/python

import csv
from  datetime import datetime
import sys
from os.path import split, splitext, join

result = []

file_path = "./evaluation/results/genetic/initBricks.genetic.out"
# "./evaluation/results/genetic/luhn-algorithm.genetic.out"


def isLogStatement( str ):
    return (str[0] == '[') and str[1:5].isdigit() and str[6:8].isdigit() and str[9:11].isdigit()

def parseLine( str ):
    strSplit = str.split(']', 1)
    timeStamp = strSplit[0][1:].split(' ')[1]
    logMessage = strSplit[1].lstrip()
    return (timeStamp, logMessage)

def parseGenTarget( str ):
    return str.split(' ')[-1][:-1]

def parseGenEntry( str ):
    words = str.split(' ')
    gen = words[3][:-1]
    fitness = words[-1][:-1]
    return (gen, fitness)

def parseIterNumber( str ):
    return str.split(' ')[2][1:]

def parseRawReport( path ):
    newIterN = curIterN = '1'
    iterations = []
    result = []
    inputfile = open(path,'r')
    targetSearches = []
    for line in inputfile:
        if not isLogStatement(line):
            continue
        (ts, logm) = parseLine(line)
        if logm.startswith("Started iteration"):
            newIterN = parseIterNumber(logm)
            if newIterN != curIterN:
                curIterN = newIterN
                result.append(iterations)
                iterations = []
            target = parseGenTarget(logm)
            targetSearches = [target]
            targetSearches.append(ts)
            continue
        if logm.startswith("Completed iteration"):
            targetSearches.append(ts)
            iterations.append(targetSearches)
        if logm.startswith("Best entity (gen."):
            entry = parseGenEntry(logm)
            targetSearches.append(entry)
    result.append(iterations)        
    inputfile.close()
    return result


def processReport(path):
    report = parseRawReport(path)
    report_new = []
    for iteration in report:
        iteration_new = []
        for target in iteration:
            stime = target[0:2][1]
            etime = target[:-3:-1][0]
            FMT = '%H:%M:%S'
            tdelta = datetime.strptime(etime, FMT) - datetime.strptime(stime, FMT)
            iteration_new.append(target[0:2] + target[:-3:-1] + [tdelta.seconds])
        report_new.append(iteration_new)
    return report_new     


def printTarget( writer, target_stats ):
    target_name = target_stats[0][0]
    start_times = target_stats[1]
    end_times = target_stats[2]
    delta_times = target_stats[4]
    zipGenFit = zip(*target_stats[3])
    generations = list(zipGenFit[0])
    fitnesses = list(zipGenFit[1])
    writer.writerow([target_name] + fitnesses)
    writer.writerow(['generations#'] + generations)
    writer.writerow(['starttime'] + start_times)
    writer.writerow(['endtime'] + end_times)
    writer.writerow(['difftime'] + delta_times)


def outputCsvReport():
    inputfile = sys.argv[1]
    print 'Analysing file:', inputfile
    (input_path, input_base) = split(inputfile)
    input_name = splitext(input_base)[0]
    outputfile = join(input_path, input_name) + '.csv'
    print 'Generating report:', outputfile
    with open(outputfile, 'w') as csvfile:
        header = ['brunch', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10']
        writer = csv.writer(csvfile, delimiter=';')
        writer.writerow(header)
        report = processReport(inputfile)
        #print report
        transp_report = map(list, zip(*report)) 
        for target in transp_report:
            transp_target = map(list, zip(*target))
            printTarget(writer, transp_target)

outputCsvReport()
            
