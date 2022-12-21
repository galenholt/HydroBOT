from py_ewr.scenario_handling import ScenarioHandler
import os
import copy
import re
import time
import pandas as pd




# function to get the names of the scenarios
def scene_namer(outerdir):
    # index 0 is the folder path, 1 is the subdirs, and 2 is files in the outer dir
    scenarios = next(os.walk(outerdir))[1]
    # allow running through additional times, and so skip the Output_files directory

    scenarios = [fold for fold in scenarios if 'Output_files' not in fold]
    
    return scenarios

# create a standard output folder structure
def make_output_dir(input_folder, scenedict):
    Output_path = os.path.join(input_folder, "Output_files", "EWR")

    for i in scenedict.keys():
        sceneout = os.path.join(Output_path, i)
        if not os.path.exists(sceneout):
            os.makedirs(sceneout)

        print("New output directories created")

        Summary_path = os.path.join(sceneout, 'summary')
        Annual_path = os.path.join(sceneout, 'annual')
        All_path = os.path.join(sceneout, 'allevents')

        if not os.path.exists(Summary_path):
            os.mkdir(Summary_path)
            print("New summary output directory created")

        if not os.path.exists(Annual_path):
            os.mkdir(Annual_path)
            print("New annual output directory created")

        if not os.path.exists(All_path):
            os.mkdir(All_path)
            print("New output directory created for all events")
    
    return Output_path
    


# this makes a dict with the directory, gauge #, and scenario name
# Is this the best way to do it? It matches everything up, but often what I need are the separate parts, and this is annoying to unfold.
def make_scenario_info(input_folder):

    scenarios = scene_namer(input_folder)

    scenedict ={}
    for sc in scenarios:
        subpath = os.path.join(input_folder, sc)
        file_names = os.listdir(subpath)
        gauges = [i.split('_', 1)[0] for i in file_names]
        file_paths = os.path.join(subpath, file_names[0])
        file_paths = [os.path.join(subpath, f) for f in file_names]

        gaugefiledict = {'gauge':gauges, 'paths':file_paths}
        scenedict[sc] = gaugefiledict
    
    return scenedict  #, file_paths

# this just unfolds the dict to get the paths
def paths_gauges(scenedict):
    file_locations = []
    gaugenum = []

    for fp in scenedict.keys():
        fl = scenedict[fp]['paths']
        g1 = scenedict[fp]['gauge']
        file_locations = file_locations + fl
        gaugenum = gaugenum + g1

    return file_locations, gaugenum

# save the new cleaner ewr structure
def save_ewrs(ewr_results, ewr_type, output_path, datesuffix = True):
    # There are some case issues between the types. Some variables have camelCase, so I don't want to goof that up, but I want 'gauge' and 'scenario' in particular to be lower. So just change the first letter
    firstlower = lambda s: s[:1].lower() + s[1:] if s else ''
    ewr_results = ewr_results.rename(columns = firstlower)
    # This is much cleaner, but I do still want to save into scenario-specific folders, I think.
    scenariopaths = ewr_results['scenario'].unique()

    # If we want a date suffix
    suff = ''
    if datesuffix:
        suff = "_" + time.strftime("%Y%m%d-%H%M%S")

    for i in scenariopaths:

        # The Scenario col is a path, need to just get the clean name        
        splitpath = i.split(os.sep)
        scenename = splitpath[-2]

        # Save
        outfile = os.path.join(output_path, scenename, ewr_type, (scenename + suff + '.csv'))
        # outfile = Output_path + "/" + gscol + "_" + time.strftime("%Y%m%d-%H%M%S") + '.csv'
        
        # Tried chaining the methods but didn't work well- some want to save, and others operate in place  

        sceneresults = ewr_results.query('scenario == @i')
        sceneresults = sceneresults.rename(columns = {'scenario': 'scenarioPath'})
        sceneresults.insert(1, 'scenario', scenename)
        sceneresults.to_csv(outfile, index = False)

def run_save_ewrs(pathlist, output_path, format, allowance, climate, outputType = 'none', datesuffix = False, returnType = False):
    thisewr = ScenarioHandler(scenario_files = pathlist, 
                         model_format = format, 
                         allowance = allowance, 
                         climate = climate)
    
    bothType = copy.deepcopy(outputType)
    bothType.extend(returnType)
    # Only calculate those parts we need
    if (('summary' in bothType) | ('everything' in bothType)):
        ewr_sum = thisewr.get_ewr_results()
    if (('annual' in bothType) | ('everything' in bothType)):
        ewr_yr = thisewr.get_yearly_ewr_results()
    if (('all' in bothType) | ('everything' in bothType)):
        ewr_all = thisewr.get_all_events()

    # only save the parts we want
    if ('summary' in outputType) | ('everything' in outputType):
        save_ewrs(ewr_sum, 'summary', output_path, datesuffix = datesuffix)
    if ('annual' in outputType) | ('everything' in outputType):
        save_ewrs(ewr_yr, 'annual', output_path, datesuffix = datesuffix)
    if ('all' in outputType) | ('everything' in outputType):
        save_ewrs(ewr_all, 'allevents', output_path, datesuffix = datesuffix)

    # Only return the parts we want
    if returnType == 'summary':
        return(ewr_sum)
    if returnType == 'annual':
        return(ewr_yr)
    if returnType == 'all':
        return(ewr_all)


# This is leftover from a bug in the EWR tool. That has been fixed, but saving this because it may come in handy for parallelisation
def loopewrs(pathlist, output_path, format, allowance, climate, datesuffix = False):

    # Let's return the summary
    allsummary = []
    for scenario in pathlist:
        thisewr = ScenarioHandler(scenario_files = [scenario], 
                         model_format = format, 
                         allowance = allowance, 
                         climate = climate)
        ewr_sum = thisewr.get_ewr_results()
        ewr_yr = thisewr.get_yearly_ewr_results()
        ewr_all = thisewr.get_all_events()
        
        allsummary.append(ewr_sum) 

        save_ewrs(ewr_sum, 'summary', output_path, datesuffix = datesuffix)
        save_ewrs(ewr_yr, 'annual', output_path, datesuffix = datesuffix)
        save_ewrs(ewr_all, 'allevents', output_path, datesuffix = datesuffix)
    
    return pd.concat(allsummary)