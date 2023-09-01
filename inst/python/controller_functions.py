from py_ewr.scenario_handling import ScenarioHandler
import os
import copy


def clean_ewrs(ewr_results, scenario_filename_split):
  # There are some case issues between the types. Some variables have camelCase, so I don't want to goof that up, but I want 'gauge' and 'scenario' in particular to be lower. So just change the first letter
    firstlower = lambda s: s[:1].lower() + s[1:] if s else ''
    ewr_results = ewr_results.rename(columns = firstlower)
    # scenarioPath tends to be redundant with scenario unless very
    # system-specific things happen. I think I'll just drop it. If we want it
    # back we should explicitly pass in a path, not hope it works based on
    # directory delimiters on different OSes. 
    # ewr_results['scenarioPath'] = ewr_results['scenario']
    
    # This assumes a naming convention of either scenarioname.csv or
    # scenarioname_gauge.csv. The R portion of the toolkit cleans up the
    # situation of scenarioname/gauge.csv, but that's best to avoid anyway 
    
    # I think this is a bad assumption, and we should let the user deal with
    # whatever their naming convention is in the output.
    ewr_results["scenario"] = ewr_results["scenario"].str.split(scenario_filename_split).str[0]
    
    return(ewr_results)

# save the new cleaner ewr structure
def save_ewrs(ewr_results, ewr_type, output_path, datesuffix = True):
    # The data comes in with different scenarios in one df, but typically the
    # scenarios will be in different directories. This sorts that out

    # If we want a date suffix
    suff = ''
    if datesuffix:
        suff = "_" + time.strftime("%Y%m%d-%H%M%S")
        
    # Get scenario names
    ewr_scenarionames = ewr_results['scenario'].unique()

    for i in ewr_scenarionames:

        outfile = os.path.join(output_path, i, ewr_type, (i + suff + '.csv'))
        # outfile = Output_path + "/" + gscol + "_" + time.strftime("%Y%m%d-%H%M%S") + '.csv'
        
        # Tried chaining the methods but didn't work well- some want to save, and others operate in place  

        sceneresults = ewr_results.query('scenario == @i')
        sceneresults.to_csv(outfile, index = False)


# Main function to run and save the EWRs
def run_save_ewrs(pathlist, output_path, model_format, allowance, climate, outputType = 'none', returnType = 'none', scenario_filename_split = '_DIRECTORYAPPEND_', datesuffix = False):
    thisewr = ScenarioHandler(scenario_files = pathlist, 
                         model_format = model_format, 
                         allowance = allowance, 
                         climate = climate)
    
    bothType = copy.deepcopy(outputType)
    bothType.extend(returnType)
    # Only calculate those parts we need
    if (('summary' in bothType) | ('everything' in bothType)):
        ewr_sum = thisewr.get_ewr_results()
        ewr_sum = clean_ewrs(ewr_sum, scenario_filename_split)
    if (('annual' in bothType) | ('everything' in bothType)):
        ewr_yr = thisewr.get_yearly_ewr_results()
        ewr_yr = clean_ewrs(ewr_sum, scenario_filename_split)
    if (('all' in bothType) | ('everything' in bothType)):
        ewr_all = thisewr.get_all_events()
        ewr_all = clean_ewrs(ewr_all, scenario_filename_split)

    # only save the parts we want
    if ('summary' in outputType) | ('everything' in outputType):
        save_ewrs(ewr_sum, 'summary', output_path, datesuffix = datesuffix)
    if ('annual' in outputType) | ('everything' in outputType):
        save_ewrs(ewr_yr, 'annual', output_path, datesuffix = datesuffix)
    if ('all' in outputType) | ('everything' in outputType):
        save_ewrs(ewr_all, 'allevents', output_path, datesuffix = datesuffix)

    # Only return the parts we want
    if 'none' in returnType:
        return None
    
    returndict = {}
    if 'summary' in returnType:
        returndict.update({ "summary" : ewr_sum})
    if 'annual' in returnType:
        returndict.update({ "annual" : ewr_yr})
    if 'all' in returnType:
        returndict.update({ "all" : ewr_all})
    
    return(returndict)
