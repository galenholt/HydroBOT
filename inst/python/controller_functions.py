import os
import copy
import zipfile
import shutil
import re

from py_ewr.scenario_handling import ScenarioHandler

def clean_ewrs(ewr_results, scenario_name):
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
    ewr_results["scenario"] = scenario_name
    
    return(ewr_results)

# save the new cleaner ewr structure
def save_ewrs(ewr_results, ewr_type, output_path, scenarios_from = 'directory', datesuffix = True):
    # The data comes in with different scenarios in one df, but typically the
    # scenarios will be in different directories. This sorts that out

    ewrresults = copy.deepcopy(ewr_results)
    # If we want a date suffix
    suff = ''
    if datesuffix:
        suff = "_" + time.strftime("%Y%m%d-%H%M%S")
        
    # Get scenario names
    ewr_scenarionames = ewrresults['scenario'].unique()

    # Now, we can toss the file names if we are getting scenario names from the directory. I was doing this in R, but we need the gauge names all the way in here.
    if scenarios_from == 'directory':
        ewrresults['scenario'] = ewrresults['scenario']#.str.replace(r'^(.*)_.*$', r'\1', regex=True)

    for i in ewr_scenarionames:
        scene_outpath = os.path.join(output_path, i)
        
        # This is a workaround for the case where we only have access to the
        # results directory and its name is not the same as the scenarios.
        if not os.path.exists(scene_outpath):
          os.makedirs(scene_outpath)
          
        outfile = os.path.join(scene_outpath, (ewr_type + suff + '.csv'))
        # outfile = Output_path + "/" + gscol + "_" + time.strftime("%Y%m%d-%H%M%S") + '.csv'
        
        # paths longer than 259 seem to fail on Windows, but do so silently. Try to be informative
        if (len(outfile) > 250) & (os.name == 'nt'):
            print(f'''path length {len(outfile)} longer than 250, may not save output. 260 is typical cutoff, +-. If saving is failing, shorten paths or disable the path length limit for your system.''')

        # If we're collapsing off gauge files, do that here for the search  
        # if scenarios_from == 'directory':
        #     i = re.sub(r'^(.*)_.*$', r'\1', i)

        sceneresults = ewrresults.query('scenario == @i')
        sceneresults.to_csv(outfile, index = False)
        # we could use `, mode='a'` in to_csv to append, but that's dangerous

# an unzipper and change pathlist to the new directory
def unzip_and_pathparse(pathlist, output_path):
        splitpath = pathlist.split('.zip/')
        splitpath[0] = splitpath[0] + '.zip'
        extract_dir = os.path.join(output_path, 'hydrozipextract')
        # Ensure the extraction directory exists, create if it doesn't
        if not os.path.exists(extract_dir):
            os.makedirs(extract_dir)
        # Open the zip file
        with zipfile.ZipFile(splitpath[0], 'r') as zip_ref:
            # Extract the single file
            zip_ref.extract(splitpath[1], path=extract_dir)
        
        # set pathlist to that directory
        pathlist = os.path.join(extract_dir, splitpath[1])
        return(pathlist)

# Main function to run and save the EWRs
def run_save_ewrs(pathlist, 
                  output_path, 
                  model_format, 
                  outputType = 'none', 
                  returnType = 'none', 
                  scenario_name = '_UNNAMEDSCENARIO_', 
                  scenarios_from = 'directory', 
                  datesuffix = False):
      
    # I'm not convinced we want to support in-function unzipping, but it would work, I guess.
    # The goal here is to unzip only the needed file, and so paralleling does that once per process
    # and we still only pass paths between functions, so the processes themselves don't have a ton of data thrash
    # still, I don't really see the point of this vs unpacking the whole zip before we start. 
    # Maybe if this were done in py-ewr, then we could read straight out of the zipped ncdf and not ever re-save it somewhere?
    # If we do go this route, should we leave the unzipped files there for later, or trash them? My inclination is trash them
    if ('.zip' in pathlist):
        pathlist = unzip_and_pathparse(pathlist, output_path)
    
    thisewr = ScenarioHandler(scenario_file = pathlist, 
                         model_format = model_format)
    
    bothType = copy.deepcopy(outputType)
    bothType.extend(returnType)
    # Only calculate those parts we need. Should be able to do this with a list
    # comprehension instead of a million ifs
    if (('summary' in bothType) | ('everything' in bothType)):
        ewr_sum = thisewr.get_ewr_results()
        ewr_sum = clean_ewrs(ewr_sum, scenario_name)
    if (('annual' in bothType) | ('everything' in bothType) | ('yearly' in bothType)):
        ewr_yr = thisewr.get_yearly_ewr_results()
        ewr_yr = clean_ewrs(ewr_yr, scenario_name)
    if (('all' in bothType) | ('everything' in bothType) | ('all_events' in bothType)):
        ewr_all = thisewr.get_all_events()
        ewr_all = clean_ewrs(ewr_all, scenario_name)
    if (('all_successful_events' in bothType) | ('everything' in bothType) | ('successful' in bothType)):
        ewr_success = thisewr.get_all_successful_events()
        ewr_success = clean_ewrs(ewr_success, scenario_name)
    if (('all_interEvents' in bothType) | ('everything' in bothType)):
        ewr_inter = thisewr.get_all_interEvents()
        ewr_inter = clean_ewrs(ewr_inter, scenario_name)
    if (('all_successful_interEvents' in bothType) | ('everything' in bothType)):
        ewr_successInter = thisewr.get_all_successful_interEvents()
        ewr_successInter = clean_ewrs(ewr_successInter, scenario_name)
    
    # cleanup the extracted zips- if the user wanted them around, they'd just unzip first, I think.
    if ('hydrozipextract' in pathlist):
        shutil.rmtree(os.path.join(output_path, 'hydrozipextract'))

    # only save the parts we want
    if ('summary' in outputType) | ('everything' in outputType):
        save_ewrs(ewr_sum, 'summary', output_path, scenarios_from = scenarios_from, datesuffix = datesuffix)
    if (('annual' in outputType) | ('everything' in outputType) | ('yearly' in outputType)):
        save_ewrs(ewr_yr, 'yearly', output_path, scenarios_from = scenarios_from, datesuffix = datesuffix)
    if ('all' in outputType) | ('everything' in outputType) | ('all_events' in outputType):
        save_ewrs(ewr_all, 'all_events', output_path, scenarios_from = scenarios_from, datesuffix = datesuffix)
    if (('all_successful_events' in outputType) | ('everything' in outputType) | ('successful' in outputType)):
        save_ewrs(ewr_success, 'all_successful_events', output_path, scenarios_from = scenarios_from, datesuffix = datesuffix)
    if (('all_interEvents' in outputType) | ('everything' in outputType)):
        save_ewrs(ewr_inter, 'all_interEvents', output_path, scenarios_from = scenarios_from, datesuffix = datesuffix)
    if (('all_successful_interEvents' in outputType) | ('everything' in outputType)):
        save_ewrs(ewr_successInter, 'all_successful_interEvents', output_path, scenarios_from = scenarios_from, datesuffix = datesuffix)
    

    # Only return the parts we want. also should be list comprehension or at
    # least piggyback the conditional logic
    if 'none' in returnType:
        return None
    
    returndict = {}
    if ('summary' in returnType) | ('everything' in returnType):
        returndict.update({ "summary" : ewr_sum})
    if (('annual' in returnType) | ('everything' in returnType) | ('yearly' in returnType)):
        returndict.update({ "yearly" : ewr_yr})
    if ('all' in returnType) | ('everything' in returnType) | ('all_events' in returnType):
        returndict.update({ "all_events" : ewr_all})
    if (('all_successful_events' in returnType) | ('everything' in returnType) | ('successful' in returnType)):
        returndict.update({ "all_successful_events" : ewr_success})
    if (('all_interEvents' in returnType) | ('everything' in returnType)):
        returndict.update({ "all_interEvents" : ewr_inter})
    if (('all_successful_interEvents' in returnType) | ('everything' in returnType)):
        returndict.update({ "all_successful_interEvents" : ewr_successInter})
    
    # Deal with directory-named. Could probably replace this with a comprehension or map
    # Have to wait until here and not do it in clean_ewrs because we need the gauges for saving unique files.
    if scenarios_from == 'directory':
        for i in returndict.keys():
            returndict[i]['scenario'] = returndict[i]['scenario']#.str.replace(r'^(.*)_.*$', r'\1', regex=True)

    return(returndict)
  
  

